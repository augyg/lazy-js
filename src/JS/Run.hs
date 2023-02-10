{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module JS.Run where

import JS.MonadJS
import JS.JS
import JS.JSFFIParse hiding (manyTill_)
import JS.Source
import JS.Types

import Text.Parsec
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad (when)
import Control.Applicative (some, liftA2)
import Language.Haskell.TH (recover)
import Data.Monoid (First(..))
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack, intercalate)
import System.Which
import System.Process (CreateProcess(..), proc, readCreateProcess, readCreateProcessWithExitCode)
import System.IO (hFlush)
import System.IO.Temp (withTempFile)
import System.Exit (ExitCode)
import Data.Text.IO (hPutStr)
import qualified Data.List

data Link = Link Text -- TODO(galen): delete

{-|

GOAL: keep jsExpression parser as is, and convert

  Function a -|
  Object a   -| ---> GenericObject a

  thus changing JSAST, lookupAST, and evalExpr (on output)

|-}

{-| CHAT-GPT feedback

The JavaScript standard library includes a number of built-in objects and functions that provide a range of services to JavaScript programs. Some of the most commonly used functions and objects in the JavaScript standard library include:

    Math: A built-in object that provides mathematical functions and constants, such as Math.abs, Math.sqrt, Math.PI, and more.

    Date: A built-in object that provides functions for working with dates and times, such as Date.now, Date.parse, and Date.prototype.toString.

    String: A built-in object that provides functions for working with strings, such as String.prototype.indexOf, String.prototype.split, and String.prototype.toLowerCase.

    Array: A built-in object that provides functions for working with arrays, such as Array.prototype.concat, Array.prototype.slice, and Array.prototype.sort.

    JSON: A built-in object that provides functions for working with JSON data, such as JSON.parse and JSON.stringify.

    Number: A built-in object that provides functions for working with numbers, such as Number.isFinite, Number.isNaN, and Number.parseFloat.

    Object: A built-in object that provides functions for working with objects, such as Object.assign, Object.create, and Object.keys.

    RegExp: A built-in object that provides functions for working with regular expressions, such as RegExp.prototype.test and RegExp.prototype.exec.

These are just a few examples of the functions and objects included in the JavaScript standard library. There are many others as well, and the full list of functions and objects can be found in the official JavaScript documentation.

|-}

  
-- evalScript :: MonadJS m => [JSTopLevel a] -> m (FullAST a)
-- evalScript = mapM_ evalTopLevel >> getAST




{-

-- { x=3; x=y; x +=1 ; z = x }

-- x is 3
-- x is y
-- x is (y + 1)
-- z is x at this step


-- I could demand for some reason (based on domain) to evaluate z (for example, lets say z :: XMLHTTPRequest)

-- I could pile up lazy evaluations by:

--   A) copy the thunk from x @ this time
--   B) Establishing two updates:
--       1) Set
--       2) Modify
--     where a Set clears house / destroys all previous updates
--        -> This is assuming that if something did set itself to (a previous value | expr(prevValue) )
--           they've already copied


-- Wait a minute, if expressions are the complicating factor, could they not copy the input values at this time


-- eg:

--   let x = f(y) -- then copy y at the time of the this line ... i guess f could change too

--   or we create a de-refd statement

--   so the let x is now:

--   let x = (function (y') { return 1 + y' })(10) -- y=10 @ time

--   but we never run this function unless we
--     A) Demand its value for an effect
--     B) Demand a value which refs x before x is destructively updated

--      eg. (with x as above)

--      g = x; x = undefined; u = x

--      then:
--         g = (function (y') { return 1 + y' })(10) -- == 11 
--         u = undefined

--      So this will really be handled when we write the expression and we can validly write the expression
--      at the time of the given line in all cases 


--    And note: these dont relate or rely on whether or not we are processing a chunk of control flow
--    rather one control flow yields a set of statements

--    And note: var is the only of the three variable declarators that can actually destructively change

--    And note: (let x = x + 1) !== (x += 1) and you cant use += on a const

-}




-- | Functions may write to their local AST and update global state
  -- | But locally declared* ObjectOriented's are not visible 

-- | 2 main jobs:
-- | 1) add to, update, take from AST
           -- | Get -> Reference
           -- | Set -> JSOperation + (class def, function def)
           -- | Modify -> ( Set:Erase | Iterate ) 
-- | 2) control when to do something =<< parser

--type ASTValue = JSExpr 

-- | todo: as StateT
lookupGlobal :: [Name] -> JSAST a -> Maybe (ExprAST a)
lookupGlobal = lookupAST 

      
--f name value = GenericObject mappy 
  



-- -- | isJust if full dot pattern was in this AST 
-- lookupAST :: [Name] -> JSAST -> Maybe ExprAST
-- lookupAST (n:nChain) (Recordc ast) =
--   case Data.List.lookup n ast of
--     Just v -> case nChain of
--       [] -> Just v
--       xs -> case v of
--         ValC (RecordC kv) -> lookupAST nChain (RecordC kv)
--         _ -> Nothing -- TODO(galen): better error handling 
--       lookupAST nChain v
--     Nothing -> Nothing
--       --error "value referenced before defining"



-- | By this reasoning about types, we can simply 'demand' values by asking for them
-- | for class objects tho, like XMLHTTPRequest, could we 
runMonadJS :: Maybe (MyAST a) -- optional pass of starting AST 
           -> [JSTopLevel a]
           -> m (JSAST a)
runMonadJS = undefined



--data JSAST = JSAST (Map Name ExprAST)



applyFunc :: (Function a) -> [Expr a] -> Expr a -- evalExpr (ApplyFunc f args) 
applyFunc = undefined




-- |  ---> inside function
-- | NOTE! This also affects if classes and functions reach global AST
evalTopLevel :: (Eq a, Fractional a, Read a, MonadJS m) => JSTopLevel a -> m (Either (ExprAST a) ())
evalTopLevel  = \case
  Control' contr -> evalControl  contr
  Declare oop -> do
    evalDeclare  oop
    pure $ Right () 
  Return expr -> Left <$> (evalExpr expr)

    -- case  of
    -- Nothing -> error "this should never happen: return outside function"
    -- Just _ -> do
    --   exprAST <- evalExpr expr 
    --   pure $ Left exprAST -- Left cuz end with this data 
-------------------------------------
  Break -> undefined -- ? -- i think im gonna do something else 
-------------------------------------          
-- | TODO(galen): var escapes scope of
evalControl :: (Eq a, Fractional a, Read a, MonadJS m) => Control a -> m (Either (ExprAST a) ())
evalControl  = \case
  While whileLoop -> evalWhileLoop whileLoop
  For forLoop -> evalForLoop forLoop 
  IF ifStatement -> evalIfStatement ifStatement
  Switch switch -> evalSwitch switch
  TryExcept tryExcFin -> evalTryExcept tryExcFin

-- | Do these last: its basically just 'should we run these top levels?'
evalWhileLoop :: (Eq a, Fractional a, Read a, MonadJS m) => WhileLoop a -> m (Either (ExprAST a) ())
evalWhileLoop = undefined

evalForLoop :: (Eq a, Fractional a, Read a, MonadJS m) => ForLoop a -> m (Either (ExprAST a) ())
evalForLoop = undefined

evalIfStatement :: (Eq a, Fractional a, Read a, MonadJS m) => IFStatement a -> m (Either (ExprAST a) ())
evalIfStatement = undefined

evalSwitch :: (Eq a, Fractional a, Read a, MonadJS m) => SwitchStatement a -> m (Either (ExprAST a) ())
evalSwitch = undefined
-- | STRICT!!!!!!!!!!!
evalTryExcept :: (Eq a, Fractional a, Read a, MonadJS m) => TryExceptFinally a -> m (Either (ExprAST a) ())
evalTryExcept = undefined

-- | IF there is no local AST then the global one will be affected
evalDeclare :: (Eq a, Fractional a, Read a, MonadJS m) => ObjectOriented a -> m () 
evalDeclare = \case
  Function' (Function mName argNames tLevels) ->
    let f = ValC . RecordC $
          GenericObject (Just $ Lambda argNames tLevels) (M.fromList [("name", ValC . StringC . JSString $ fromMaybe "" mName)])
    in case mName of
      Nothing -> error "encountered top level function def with no name"
      Just name -> putNewEntryASTs' [name] f
      
  Class cDef@(JSClass mName _ _ _) -> case mName of
    Nothing -> error "encountered top level class def with no name"
    Just name -> putNewEntryASTs' [name] (ClassAST cDef)
    
  Operation operation -> evalJSOp operation -- VOILA!


      -------------------------------------------------
      -- SO WHEN WE ENTER AND EXIT A FUNCTION WE JUST CHANGE THE AST
      -- IN EG.: (Just newASTFromArgs, fromJust lastInner : innerASTs, global)
      -- OUT EG.(from In eg.): (Just $ head innerASTs, tail innerASTs, global) 





-- | TODO(galen): Can this handle setting a new property ? this.x = 1?     

-- | Eval to WHNF with freezed values from the references
-- | evalExpr should only affect a JSAST through evalOp and never directly itself
-- | however this doesn't mean that if the expression is function app for example, that
-- | it can't modify state
-- | SO: we should no new keys after evalExpr in theory
evalJSOp :: (Eq a, Fractional a, Read a, MonadJS m) => JSOperation a -> m ()
evalJSOp (JSOperation _ varDecl expr) = do
  exprOut <- evalExpr expr
  case varDecl of
    Nothing -> pure () -- the evaluation of expression may have updated an Object tho
    Just (Let name) -> putNewEntryASTs' [name] exprOut
    Just (Var name) -> putNewEntryASTs' [name] exprOut
    Just (Const name) -> putNewEntryASTs' [name] exprOut
    Just (Raw A_Equal name) -> putNewEntryASTs' name exprOut
    Just (Raw opA nameChain) -> do 
      exprC <- lookupASTs nameChain -- TODO(galen): lens for maps
      case exprC of
        Just exprC' -> modifyEntryASTs' nameChain $ PureOp (toAssocOperator opA) exprC' exprOut
        Nothing -> error "attempted to modify missing variable"
      

toAssocOperator :: AssignOp -> Operator 
toAssocOperator = \case
  A_Equal -> Equal -- Never should be used tho
  A_Plus -> Plus
  A_Subtract -> Subtract
  A_Multiply -> Multiply
  A_Divide -> Division 
  A_Modulo -> Modulo
  A_Exponentiation -> Exponentiation
  A_LShift -> Shift "<<="
  A_RShift -> Shift ">>="
  A_UnsignedRShift -> error "idc enough"
  A_ANDB -> ANDB
  A_XORB -> XORB--- ^=
  A_ORB -> ORB
  A_AND -> AND 
  A_OR -> OR 
  A_Nullish -> undefined -- x ? x : 2
    -- may just need to handle specially by checking if jsUndefined or Null 

-- | Can only affect innermost scope 
putNewEntryASTs' :: MonadJS m => NameChain -> ExprAST a -> m ()
putNewEntryASTs' nameChain exprC = do
  (mInner, inters, global) <- getAST
  case mInner of
    Just astI ->
      let astI' = putNewEntryAST' nameChain exprC astI
      in putAST (Just astI', inters, global)
    Nothing ->
      let global' = putNewEntryAST' nameChain exprC global
      in putAST (mInner, inters, global')


putNewEntryAST' :: [Name] -> ExprAST a -> JSAST a -> JSAST a
putNewEntryAST' (n:[]) exprC (GenericObject l kv) =
  -- TODO(galen): for let and const, check if they already exist
  GenericObject l $ M.insert n exprC kv  
putNewEntryAST' (n:nameChain) exprC (GenericObject l kv) =
  case M.lookup n kv of
    Just exprC -> case exprC of
      ValC (RecordC obj) -> --(GenericObject l2 obj)) ->
        let GenericObject l2 newInnerObj = putNewEntryAST' nameChain exprC obj
        in
          --TODO(): better; more efficient 
          GenericObject l $ M.insert n (ValC $ RecordC $ GenericObject l2 newInnerObj) kv
      

-- | isJust if full dot pattern was in this AST 
lookupAST :: [Name] -> JSAST a -> Maybe (ExprAST a)
lookupAST (n:nChain) (GenericObject l ast) = --  {-:: GenericObject a-}) =
  case M.lookup n ast of
    Just v -> case nChain of
      [] -> Just v
      xs -> case v of
        ValC (RecordC kv) -> lookupAST nChain kv
        _ -> Nothing -- TODO(galen): better error handling 
      --lookupAST nChain v
    Nothing -> Nothing
      --error "value referenced before defining"


lookupASTs :: MonadJS m => NameChain -> m (Maybe (ExprAST a))
lookupASTs nameChain = do
  (mInner, inters, global) <- getAST
  case head nameChain == "this" of
    True -> case mInner of
      Nothing -> pure $ lookupAST nameChain global
      Just astI -> case lookupAST nameChain astI of
        Nothing -> pure $ lookupAST nameChain global
        Just match -> pure $ Just match
    False -> case mInner of
      Just astI -> case lookupAST nameChain astI of  --pure $ Just match
        Just match' -> pure $ Just match'
        Nothing -> case getFirst . mconcat $ First . (lookupAST nameChain) <$> inters of
          Just match' -> pure $ Just match'
          Nothing -> pure $ lookupAST nameChain global 
  
      Nothing -> case getFirst . mconcat $ First . (lookupAST nameChain) <$> inters of
        Just match' -> pure $ Just match'
        Nothing -> pure $ lookupAST nameChain global 
  
prefix = Data.List.init 

-- case length nameChain == 1 of
--   True -> -- get from immediate object 

--updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a) 


         
modifyEntryAST' :: [Name] -> ExprAST a -> JSAST a -> (Maybe (ExprAST a), JSAST a)
modifyEntryAST' (n:[]) toPut (GenericObject l kv) =
  let (s, mappy) = M.updateLookupWithKey (\_ v -> Just toPut) n kv -- toPut
  in (s, GenericObject l mappy)
modifyEntryAST' (n:nameChain) toPut (GenericObject l kv) = -- not at last
  let
    -- analyze the key of our third arg 

    (s,mappy) = M.updateLookupWithKey (modifyFunc nameChain toPut) n kv 
  in 
    (s, GenericObject l mappy)

modifyFunc :: NameChain -> ExprAST a -> Name -> ExprAST a -> Maybe (ExprAST a)
modifyFunc nameChain toPut _ = \case
  ClassAST _ -> Nothing 
  PureOp _ _ _ -> Nothing 
  ValC (RecordC obj) ->
    let (mSuccess, kv_updated) = modifyEntryAST' nameChain toPut obj
    in case mSuccess of
      Just _ -> Just . ValC . RecordC $ kv_updated
      Nothing -> Nothing 


-- | Should move to MonadJS

handleInters :: (Fractional a, Read a) => NameChain -> ExprAST a -> [MyAST a] -> [(Maybe (ExprAST a), MyAST a)]
handleInters _ _ [] = [] 
handleInters nChain toPut (ast:asts) =
  let x@(mSucc, nAST) = modifyEntryAST' nChain toPut ast
  in case mSucc of
    Just _ -> x: (fmap (Nothing,) asts)
    Nothing -> x : (handleInters nChain toPut asts)

modifyEntryASTs' :: (Eq a, Fractional a, Read a, MonadJS m) => NameChain -> ExprAST a -> m () -- FullAST a
modifyEntryASTs' nameChain toPut = do
  (mInner, inters, global) <- getAST
  case head nameChain == "this" of
    True -> case mInner of
      Nothing -> do 
        let (mChange, newGlobal) = modifyEntryAST' nameChain toPut global
        when (mChange == Nothing) $ error "tried to modify non-existent entry" 
        putAST (mInner, inters, newGlobal)
      Just astI -> do 
        let (mChange, newASTI) = modifyEntryAST' nameChain toPut astI
        when (mChange == Nothing) $ error "tried to modify non-existent entry" 
        putAST (Just newASTI, inters, global)
    False -> case mInner of
      Just astI -> do 
        let (mChange, newASTI) = modifyEntryAST' nameChain toPut astI
        --when (mChange == Nothing) $ error "tried to modify non-existent entry" 
        case mChange of
          Just _ -> putAST (Just newASTI, inters, global)
          Nothing ->  do
            let (succ, inters') = unzip $ handleInters nameChain toPut inters
            let succeeded = any isJust succ
            case succeeded of
              True -> putAST (mInner, inters', global)
              False -> do
                let (mChange, newGlobal) = modifyEntryAST' nameChain toPut global
                when (mChange == Nothing) $ error "tried to modify non-existent entry" 
                putAST (mInner, inters, newGlobal)
      Nothing -> do
        let (succ, inters') = unzip $ handleInters nameChain toPut inters
        let succeeded = any isJust succ
        case succeeded of
          True -> putAST (mInner, inters', global)
          False -> do
            let (mChange, newGlobal) = modifyEntryAST' nameChain toPut global
            when (mChange == Nothing) $ error "tried to modify non-existent entry" 
            putAST (mInner, inters, newGlobal)



-- | This returns something that is unaffected by global state
-- | TODO(galen): can we confirm that all illogical expressions that are syntacticall valid give JSUndefined?
evalExpr :: (Fractional a, Eq a, Read a, MonadJS m) => Expr a -> m (ExprAST a)
evalExpr = \case
  
  Reference r -> do
    let shouldHijack = undefined -- check for standard library functions we havent implemented
    let runHijack = undefined
    case shouldHijack r of
      True -> runHijack r
      False -> evalRef r
    
  -- | we may need to make the '[Function: name]' and '[class name]' strings if they are these cases
  -- | but in this far, this is valid and we dont need to care 
  Op operator expr1 expr2 -> do
    pureE1 <- evalExpr expr1 
    pureE2 <- evalExpr expr2
    pure $ PureOp operator pureE1 pureE2  

  -------------------------------------------------
  --- All of these are ready to be put to global state 
  Val v -> 
    fmap ValC $ case v of
    -- | Need to make Objects and Arrays WHNF
      Number n -> pure $ NumberC n 
      String' s -> pure $ StringC s 
      Null n -> pure $ NullC n
      Boolean b -> pure $ BooleanC b
      -- | TODO(galen): LookupArray (Array a) (Expr a) --> undefined if not Int
      Array (JSArray exprs) -> do
        -- | TODO(galen): should we be even lazier by just binding in a tuple with the current AST 
        exprCs <- mapM evalExpr exprs
        pure $ ArrayC $ JSArrayC exprCs
      -- | TODO(galen): LookupObject (Array a) (Expr a) --> undefined if not string
      Record (JSObject fields) -> do
        fields' <- mapM (\(name,e) -> (name,) <$> (evalExpr e)) fields
        pure . RecordC $ GenericObject Nothing $ M.fromList fields'

            
      JSUndefined -> pure JSUndefinedC 
--        pure $ ValC val
  -- | put as an object (is value)
  -- | TODO: Impl. naming precedence: with eg: (x=function f(){}) f > x > '' 
  FuncAsExpr (Function mName argNames tLevels) ->
    pure . ValC . RecordC $
    GenericObject (Just $ Lambda argNames tLevels) (M.fromList [("name", ValC . StringC . JSString $ fromMaybe "" mName)])
    --pure $ FuncAST f 
  ClassAsExpr class' -> pure $ ClassAST class'
  -- | The func used here doesnt affect the AST but the result still will
  -- | this is distinct from evalRef in that we use evalLambda not evalFunctionProperty
  -- | even though a function is technically an Object
  ApplyFunc eithFuncy argExprs -> do
    -- | The value returned will be the Object given to the 'return' keyword
    argsPure <- mapM evalExpr argExprs
    case eithFuncy of
      Right (Function _ argNames tLevels) -> evalLambda (Lambda argNames tLevels) argsPure 
      Left name -> do
        mLambda <- lookupASTs [name]
        case mLambda of
          Just (ValC (RecordC (GenericObject (Just lambda) _))) -> evalLambda lambda argsPure
          Nothing -> error "Object does not exist" 
          _ -> error "Object is not callable"
            

  -- | TODO!
  -- | TODO: New (Expr a) [Expr a] because: new {a : class {} }.a(1)
  -- | and: new (class A {})()
  -- | SO!
  -- | This should handle an expression in a special way if needed
    -- | until it gets a class def
  -- this can also work with function instead of an actual class def
  -- | TODO(galen): set .constructor field to '[class <Name>]'
  -- | 
  -- | NOTE! fakeObject = { super : { f : function() {}, g : function() {} }, this : { x : 1}}
  -- | this.x == 1
  -- | super.f `oftype` function
  -- | super.g `oftype` function
  -- | When objects are run in a functional-dependent context (evalFuncProperty)
  -- | then super stays the same (it just sits there until we use it) ((wait, is it usable in plain ctx?))
    -- | we just need to put them there when we create the object 
  -- | and all object properties become wrapped in 'this' reference 
  New className argExprs -> undefined
    -- commented out for compiler (see below)
----------------------------------------------------------------------------------------------------------------

    -- objC <- createNewObject className argExprs
    -- pure $ ValC objC
    -- -- | 1. fold methods into proper nameSpaces (these aren't eval'd yet, but they may be used) 
    -- -- | 2. call base constructor 
        
    -- where
    --   -- data JSClass a = JSClass (Maybe Extends) (Maybe (Constructor a)) [Method a] deriving (Show, Eq, Ord)
    --   createNewObject' :: JSClass -> [ExprAST a] -> m (GenericObject a)
    --   createNewObject' (JSClass mExtends mConstructor methods) args = do
    --     let super = case mExtends of --if has extends then set super key
    --           Just (JSClass mExtends mConstructor methods) -> Just $ constructorToLambda mConstructor
    --           Nothing -> Nothing
    --     let superMethods = case mExtends of  -- by folding
    --           Just (JSClass mExtends mConstructor methods) -> fmap (nameF . coerceMethodToFunc) methods)

    --     let superObjFunc = GenericObject super superMethods 
            
    --     let
    --       thisMethods :: [(Name, ExprAST a)]
    --       thisMethods = fmap (nameF . coerceMethodToFunc) methods
              
    --     (_, this) <- tryApplyPropertyFunction emptyJSRecord Nothing args
            
          
    --   nameF :: Function a -> (Name, ExprAST a)
    --   nameF f@(Function (Just methodName) args tLevs) = (methodName, FuncAST f)


          
    --   callsSuper :: [JSTopLevel a] -> Bool
    --   callsSuper = any f
    --     where
    --       f :: JSTopLevel a -> Bool
    --       f = \case
    --         Control _ -> False
    --         Return _ -> False
    --         Break -> False 
    --         Declare oo -> case oo of
    --           Function' _ -> False
    --           Class _ -> False
    --           Operation (JSOperation _ _ e) -> case e of
    --             ApplyFunc (Left "super") _ -> True -- only case lol
    --             _ -> False


          



          
    --   runConstructor :: MonadJS m => Method a -> Maybe Extends -> m (GenericObject a)
    --   runConstructor (Method _ argNames tLevels) mExtends = do
    --     case mExtends of
    --       Nothing -> ""
    --       Just parentName -> 
    --         case callsSuper tLevels of
    --           True -> do  -- get super from parent and add this
    --             (JSClass mExtends (Method _ argNamesExt tLevelsExt methods)) <- lookupASTs parentName
                    
    --           False -> "" -- simple
              


          
    --   -- runConstructor :: MonadJS m => Method a -> Maybe Extends -> m (GenericObject a)
    --   -- runConstructor (Method _ argNames tLevels) = do
    --   --   case extends of
    --   --     Just parentName -> do
    --   --       (JSClass mExtends mConstructor methods) <- lookupASTs parentName
    --   --       case mConstructor of
    --   --         Just constructor ->
    --   --           addArgsToASTScope [ArgName "super"]
                    
    --   --         Nothing -> pure () 
    --   --     Nothing -> 
            

          
    --   -- | NOTE: one fold is the difference between this and super 
    --     -- | so fold all but the last to get super
    --     -- | then fold this with the last/immediate class to get 'this'
            
    --   foldMethods :: [[Method a]] -> Map Name (Method a)
    --   foldMethods = foldr f mempty 
          
    --   f :: [Method a] -> [Method a] -> Map Name (Method a)
    --   f parent child =
    --     let
    --       parent' = M.fromList $ nameF <$> parent
    --       child' = M.fromList $ nameF <$> child
    --     in
    --       M.union child' parent' 

----------------------------------------------------------------------------------------------------------------
        
    -- if we have an extends: put super as the parent constructor

    --   or the constructor will have a list of top levels, we could simply just replace it with
    --   the super 
        
--         where
--           -- Name results in a list of classes to fold, that can then be run as a function of sorts
--           fakeCase :: JSClass a -> [Expr a] -> m GenericObject
--           fakeCase (JSClass _ mConstructor _) exprs = do
--             case mConstructor of
--               Just c -> do
--                 _ {-will never be anything-} <- evalExpr $ ApplyFunc c exprs
--               Nothing -> do
--                 pure ()

--             addMethodsToThis 
            
-- function that can refer to internal state


-- could we not call the chained function with also it's own internal state attached in the tree? 

-- super is also this
-- x is also this.x
-- this.x is also x 

--           createNewObject :: Name -> [Expr a] -> m GenericObject
--           createNewObject className argExprs = do
--             argsPure <- mapM evalExpr argExprs

--             could we fake this via a function-like interface
--             where we add to an object 'this'

--             let fakeAST = GenericObject [("this", GenericObject []), ("super", superConstructor)]
--             let thisToObject (GenericObject [("this", GenericObject props):otherNameSpaces]) = GenericObject props


--             coerceToFunctionReturningThis :: [JSClass a] -> [Expr a] -> ApplyFunc -- (-> GenericObject)

--             parent methods are available in 3 ways:
--             super.method
--             this.method
--             method

--             Maybe we should copy functions to both this and super keys




--             except all properties that are values are only accessible via this

--             I think this is cuz 
            
--             -- declare all 

--             -- FALSE: cuz all the fields would be the same 
--             -- But how do we handle

--             -- let y = new A()
--             -- let z = y --> will still refer to y.<someProp> 
            
--             shouldBeClass <- getAST className
--             case shouldBeClass of
--               ClassAST (JSClass mExtends mConstructor methods) -> do
--                 parentClasses <- getParentClasses mExtends
--                 super <- foldClasses parentClasses 
--                 -- get all methods as a name 'super' set to being field in type ObjectC

--                 -- this should work so long as we can fold it in, that for what we are instantiating, for name conflicts
--                 -- there is only one right answer: a parent or a child

--                   -- Child overrides the parent method
--                       -- so for any inner naming, there is only one correct ref 

--                 super() --> parent constructor(s)
--                 super._something_ --> parent property (function only) 
                
                
--               _ -> error "type error: new used on non-class"

--           replaceSuper = do -- "super() ----> runParentConstructor pConstr"
--             case topLevel of
--               Declare (Operation (JSOperation _ _ (ApplyFunc (Left "super") args))) ->
--                 evalExpr <- ApplyFunc (toFunc parentConstructor) args
--                 -- but this would create a this object in state with a number of properties
                
          
--           foldClasses :: [JSClass a] -> JSClass a
--           foldClasses [] = 
--           foldClasses (c:cs) = do
--             let child = JSClass (Just parent) (Just constructorWithParentRef) methodsC
--             let parent = JSClass Nothing (Just constructor) methodsP


--             let methods' = union child parent

--             the constructor may have a super reference
--             -- ***In a child class constructor*** 'this' cannot be used until 'super' is called.

--             super is the parents constructor and may define some properties 
            
--             otherwise: we have no way of getting any parent-based properties 

--             parent.constructor = constructor (a@_manyArgs_) { this.x = <v> ; this.y = <v2> }
--             child .constructor = constructor (a2, aArgs) { super(aArgs); this.z = this.y + this.x

--             So basically the constructors unfold into a set of expressions about this

--             so a method in child should have access to all methods and all attributes created by the parent and child

            

--             case childConstructor of
--               Just constructorC -> ""
--               Nothing -> "" 
            
                  
            

       
--           getParentClasses :: Maybe Name -> m [JSClass a]
--           getParentClasses maybeName = do
--             case maybeName of
--               Nothing -> pure []
--               Just thisClass@(JSClass mExtends _ _) -> do
--                 manyClasses <- getParentClasses mExtends
--                 pure $ thisClass : manyClasses

                                
                


evalLambda :: (MonadJS m, Eq a, Fractional a, Read a) => Lambda a -> [ExprAST a] -> m (ExprAST a)
evalLambda f@(Lambda argNames tLevels) argExprsPure = do
  addArgsToASTScope argNames argExprsPure --
  eithExprAST <- evalFuncInner tLevels
  removeDeclarationsFromScope 
  case eithExprAST of
    Right () -> -- never returned 
      pure $ ValC JSUndefinedC
    Left returned ->
      pure returned

evalLambdaProperty :: (MonadJS m, Eq a, Fractional a, Read a) => Lambda a -> [ExprAST a] -> m (ExprAST a, GenericObject a)
evalLambdaProperty f@(Lambda argNames tLevels) argExprsPure = do
  addArgsToASTScopeThis argNames argExprsPure --
  eithExprAST <- evalFuncInner tLevels
  this <- getThis 
  removeDeclarationsFromScope 
  case eithExprAST of
    Right () -> -- never returned 
      pure (ValC JSUndefinedC, this)
    Left returned ->
      pure (returned, this)
  where
    getThis :: MonadJS m => m (GenericObject a)
    getThis = do
      (mInner, inters, GenericObject l g) <- getAST
      case mInner of
        Nothing -> error "found no inner scope while evaluating function"
        Just astI -> pure . (\(ValC (RecordC obj)) -> obj) . fromJust . (lookupAST ["this"]) $ astI


  
evalFuncInner :: (MonadJS m, Eq a, Fractional a, Read a) => [JSTopLevel a] -> m (Either (ExprAST a) ())
evalFuncInner [] = pure $ Right ()
evalFuncInner (tLevel:tLevels) = do
  eithReturned :: Either (ExprAST a) () <- evalTopLevel tLevel
  case eithReturned of
    Left exprC ->
      pure $ Left exprC -- this is not a fail tho
    Right () ->
      evalFuncInner tLevels
      
zipArgs :: (MonadJS m, Eq a, Fractional a, Read a) => [ArgName a] -> [ExprAST a] -> m [(Name,ExprAST a)]
zipArgs [] _ = pure []
zipArgs (argName:argNames) [] = case argName of
  ArgName _ -> zipArgs argNames []
  ArgDef name expr -> do
    exprC <- evalExpr expr
    nExprs <- zipArgs argNames []
    pure $ (name, exprC) : nExprs
    --liftA2 (:) (pure (name,exprC)) (zipArgs argNames [])
zipArgs (argName:argNames) (exprC:exprCs) =
  let
    toName = \case { ArgName n -> n; ArgDef n _ -> n }
  in do 
    nExprs <- zipArgs argNames exprCs
    pure $ (toName argName, exprC) : nExprs
    

-- By this point we've already eval'd the expr args
addArgsToASTScope :: (MonadJS m, Eq a, Fractional a, Read a) => [ArgName a] -> [ExprAST a] -> m ()
addArgsToASTScope argNames exprCs = do
  newAST <- M.fromList <$> zipArgs argNames exprCs
  (mInner, inters, g) <- getAST
  case mInner of
    Just astI -> 
      putAST (Just $ GenericObject Nothing newAST, astI : inters, g) 
    Nothing ->
      case inters of
        (x:xs) -> error "invalid function-scope state handling" 
        [] -> --logical
          putAST (Just $ GenericObject Nothing  newAST, [], g)
  
-- Allow infinite nesting without scoping inwards, instead just referring to the same 'this'
addArgsToASTScopeThis :: (MonadJS m, Eq a, Fractional a, Read a) => [ArgName a] -> [ExprAST a] -> m ()
addArgsToASTScopeThis argNames exprs = do
  st@(mInner, inters, g) <- getAST
  args_ <- zipArgs argNames exprs
  case mInner of
    -- 'this' cannot possibly exist
    Nothing -> definitelyNoExistingThis args_ st 
    Just obj -> existingThis args_ obj st

  where
    definitelyNoExistingThis args_ (mInner, inters, g) = case inters of
      (x:xs) -> error "invalid handling of state"
      [] -> putAST (Just $ GenericObject Nothing (M.fromList args_), [], g)

    existingThis args_ (GenericObject l astI) (mInner, inters, g) = do 
      let (mSuperThisExisting, astI') = M.partitionWithKey (\k v -> k == "this" || k == "super") astI
      case M.lookup "this" mSuperThisExisting of
        Nothing ->
          -- 'this' doesn't exist in existing inner tree 
          putAST (Just $ GenericObject Nothing (M.fromList args_), (GenericObject l astI): inters, g) 
        Just this' -> do
          -- this already exists, so add to it, prolly same with super
          let astInnerNew = (M.unionWith unionThisSuper mSuperThisExisting (M.fromList args_))
          putAST (Just $ GenericObject Nothing astInnerNew, (GenericObject l astI'):inters, g)

-- | It is possible that lambdas may exist cuz of super() however we ensure it doesn't go further from here
-- | by setting to Nothing
unionThisSuper :: ExprAST a -> ExprAST a -> ExprAST a 
unionThisSuper existing@(ValC (RecordC (GenericObject _ thisOld))) patch@(ValC (RecordC (GenericObject _ thisNew))) =
  ValC $ RecordC (GenericObject Nothing (M.union thisNew thisOld))


-- | Step out one level of scope 
removeDeclarationsFromScope :: MonadJS m => m ()
removeDeclarationsFromScope = do
  (_, inters, g) <- getAST
  case inters of
    [] -> putAST (Nothing, [], g) -- not in a function now 
    (x:xs) -> putAST (Just x, xs, g) 



-- evalFuncProperty :: Function a -> [ExprAST a] -> m (ExprAST a)
-- evalFuncProperty f@(Function mName argNames tLevels) argExprsPure = do
--   addArgsToASTScope argNames argExprsPure --
--   eithExprAST <- evalFunc'
--   this <- getThis 
--   removeDeclarationsFromScope 
--   case eithExprAST of
--     Right () -> -- never returned 
--       pure $ Val JSUndefinedC
--     Left returned ->
--       pure returned
--   where
--     getThis :: MonadJS m => m ()
--     getThis = do
--       (mInner, inters, GenericObject g) <- getAST
--       case mInner of
--         Nothing -> error "found no inner scope while evaluating function"
--         Just astI -> pure . fromJust . (lookupAST ["this"]) $ astI
    
--     removeDeclarationsFromScope :: MonadJS m => m ()
--     removeDeclarationsFromScope = do
--       (mInner, inters, GenericObject g) <- getAST 
--       case mInner of
--         Nothing -> error "removal of non-existent AST upon completion of funcProp"
--         Just astI ->
--           let this = lookupAST ["this"] astI
--               -- super = lookupAST ["super"] astI
--           in case inters of
--                [] -> putASTs (Nothing, [], GenericObject (
          
      
--     evalFunc' :: [JSTopLevel a] -> m (Either ObjectOriented ()) 
--     evalFunc' (tLevel:tLevels) = do
--       eithReturned :: Either (ExprAST a) () <- evalTopLevel tLevel
--       case eithReturned of
--         Left exprC ->
--           pure exprC
--         Right () ->
--           evalFunc' tLevels


-- handleThis


-- so while it is true that we could have a this reference which refers to two possible cases,
-- we can deterministically rule out the possibility of it being the inner AST if

-- A) mInner == Nothing
-- B) Just astI >>= lookupAST ["this"] == Nothing


-- Also we cannot surprisingly enter nested functions

-- eg:
--   iter() is a method

-- some other method cannot do:

--   f() { x = iter } ; it must do:  f() { x = this.iter } 


-- Although how would double nested 'this' work?


-- -- | TODO(remove InObject)
-- lookupASTs :: [Name] -> (Maybe JSAST, [MyAST], JSAST) -> Maybe (ExprAST a)
-- lookupASTs (n:nameChain) (mInner, inters, global) =
--   -- "this" -> case inObject of
--   --   False -> lookupGlobal nameChain global --todo: as StateT
--   --   True -> case mInner of
--   --     Nothing -> Nothing
--   --     Just astI -> lookupAST (n:nameChain) astI
--   -- _ ->
--   case mInner of
--     Nothing -> -- we must be in global
--       case inters of
--         (x:xs) -> error "invalid handling of AST detected"
--         [] -> lookupGlobal global 
--     Just astI -> case lookupAST (n:nameChain) astI of
--       Just e -> Just e 
--       Nothing -> case getFirst $ First . (lookupAST (n:nameChain)) <$> inters of
--         Just e -> Just e 
--         Nothing -> lookupGlobal (n:nameChain) global

-- modifyEntryAST' :: NameC      
-- modifyEntryAST' nameChain = do
--   (mInner, inters, global) <- getAST
--   case head nameChain == "this" of
--     True -> case mInner of
--       Nothing -> case lookupAST (prefix nameChain) global of
--         Just match -> 
        
--       Just astI -> case lookupAST nameChain astI of
--         Nothing -> pure $ lookupAST nameChain global
--         Just match -> pure $ Just match
--     False -> case mInner of
--       Just match -> pure $ Just match
--       Nothing -> case getFirst $ First . (lookupAST nameChain) <$> inters of
--         Just match' -> pure $ Just match'
--         Nothing -> pure $ lookupAST nameChain global 


  -- case head nameChain == "this" of
  --   True -> handleThis nameChain mArgs mChain
  --   False -> -- normal rules apply 
  --     case lookupASTs nameChain asts of
  --       Nothing -> error $ "evalRef couldnt find: " <> (show nameChain)
  --       Just exprC -> handleFoundLookup nameChain exprC mArgs

  -- where
  --   handleThis :: [Name]  {-(Ref a)-}
  --              -> Maybe [Expr a]
  --              -> Maybe (Ref a) 
  --              -> m (ExprAST a)
  --   handleThis nameChain mArgs mChain = do
  --     asts@(mInnerScope, interScopes, global) <- getAST
  --     let globalLook =
  --           case lookupAST nameChain global of
  --             Just exprC -> handleFoundLookup nameChain exprC mArgs
  --             Nothing -> error $ "ref using 'this' keyword not found:" <> (show nameChain)
  --     case mInnerScope of
  --       Nothing -> globalLook -- is global
  --       Just astI -> case lookupAST nameChain astI of
  --         Just exprC -> handleFoundLookup nameChain exprC mArgs
  --         -- just in normal function
  --         -- TODO: check if 'this'  exists in astI (for testing sake really)
  --         Nothing -> globalLook  


-- | NOTE: fakeObject = { super : { f : function() {}, g : function() {} }, this : { x : 1}}
evalRef :: (MonadJS m, Eq a, Fractional a, Read a) => Ref a -> m (ExprAST a)
evalRef r@(Ref nameChain deps mArgs mChain) = do
  -- TODO: refactor
  asts@(mInnerScope, interScopes, global) <- getAST

  exprC <- lookupASTs nameChain
  case exprC of
    Nothing -> error $ "lookup of non-existent ref: " <> (show nameChain)
    Just exprC' -> handleFoundLookup nameChain exprC' mArgs mChain 
      
    
--handleFoundLookup :: Ref -> ExprAST a -> m (ExprAST a)
handleFoundLookup :: (MonadJS m, Eq a, Fractional a, Read a) =>
                     [Name]
                  -> ExprAST a 
                  -> Maybe [Expr a]
                  -> Maybe (Ref a)
                  -> m (ExprAST a)
handleFoundLookup nameChain exprC mArgs mChain = -- (Ref nameChain _ mArgs mChain) exprC = 
  case mArgs of
    Nothing -> pure exprC  
    Just args -> do
      let allButLast = Data.List.init nameChain
      oFrom <- lookupASTs allButLast
      case oFrom of
        Nothing -> error "this should be impossible"
        Just (ValC (RecordC objectFrom)) -> 
          case exprC of
            ValC (RecordC (GenericObject (Just lambda) propertiesF)) -> do
              --FuncAST f -> 
              (returned, this') <- tryApplyPropertyFunction objectFrom lambda args
              modifyEntryAST allButLast (ValC $ RecordC this')
              chainExprMaybe returned mChain

            ClassAST _ -> erroridcclass
            _ -> error "type error: applied args to non function"
          
-- factoring out helper
tryApplyPropertyFunction :: (Fractional a, Eq a, Read a, MonadJS m) =>
                            GenericObject a
                         -> Lambda a 
                         -> [Expr a]
                         -> m (ExprAST a, GenericObject a)
tryApplyPropertyFunction objectFrom (Lambda argNames tLevels) args = do
  args' :: [ExprAST a] <- mapM evalExpr args
  let f' = (Lambda ((ArgName "this") : (ArgName "super") :argNames) tLevels)
  let super = fromMaybe (ValC $ RecordC $ GenericObject Nothing mempty) (lookupAST ["super"] objectFrom)
  let args'' = (ValC (RecordC objectFrom)) : ( super ) : args'
  evalLambdaProperty f' args'

-- | we may need to run node in this case and with String
-- (function f() {}).name -> 'f'
chainExprMaybe :: (MonadJS m, Eq a, Fractional a, Read a) => ExprAST a -> Maybe (Ref a) -> m (ExprAST a)
chainExprMaybe exprC = \case 
  Nothing -> pure exprC
  Just (chainRef :: Ref a) -> case exprC of
    ValC (RecordC jsRecordC') -> evalChainedRef jsRecordC' chainRef
    ValC (StringC s) -> error "unimplemented" -- "".length()
    --FuncAST f -> error "unimplemented" -- IMPLEMENTED!
    _ -> error "type error: applied chain to unchainable type"
    
evalChainedRef :: (MonadJS m, Eq a,  Fractional a, Read a) => GenericObject a -> Ref a -> m (ExprAST a)
evalChainedRef object (Ref nameChain deps mArgs mChain) = do
  case lookupAST nameChain object of
    Nothing -> error "chain on non-existent property"
    Just exprAST ->
--          handleFoundLookup nameChain 
      case mArgs of
        Nothing -> pure exprAST
        Just argsPassed -> case exprAST of 
          ValC (RecordC (GenericObject (Just lambda) propertiesF)) -> do
            --FuncAST f ->
                
            -- is impossible for 'this' here to affect global state
            -- at least on this Statement; we could set a variable equal to the returned value
            -- then it would have capacity to affect the ASTs
            let allButLast = Data.List.init nameChain
            let ValC (RecordC objectFrom) = fromJust $ lookupAST allButLast object
            (returned, _) <- tryApplyPropertyFunction objectFrom lambda argsPassed
            chainExprMaybe returned mChain 
 
          ClassAST _ -> erroridcclass
          _ -> error "type error: applied args to non function"

              ---(returned, this') <- tryApplyPropertyFunction objectFrom lambda args


      
      
      
      -- | best way to set this up is to allow infinite recursion (and therefore also simple lookups)
      -- | and then check if its a func; if it is, then apply args if not (== [])
      -- | then if we have args for a func: check if we chain (we can now assume the return-ed is an Object)
         -- | DoChain --> pass return statement to chain statement
               -- | Only ns@[Name] ~~ Property Grab from (Return x) --> \x -> evalRefObj ns x
               -- | [Name] <+> FnCall ~~ Property Grab from (Return x) --> \x -> if isFn then evalFunc x args 
                    -- | Except since this may infinitely repeat, call evalRef which will call evalFunc
    
    
erroridcclass = error $ "not implemented, maybe no valid implementation here anyways(see comment)"
                <> "JS: Uncaught TypeError: Class constructor A cannot be invoked without 'new'"
                -- would need to be preceded with new keyword, so idk if this
                -- case will actually exist / is gonna be an error


--type DepExpr a = (Expr a, [Dependency])


-- Should we ever copy AST for a massive deRef? In case that never needs to happen?
  -- I mean, it would still be lazy `\_o_/`
  -- like an array which parses to be quite intensive (eg. 10000s of refs) 

-- data JSASTValue a = Pure a
--                   | Object (ObjectOriented a) 

-- deRefExample = do
--   let jsAST = [("x", "10")] 
  
--   let Right (expr:[]) = parse jsOperation "" "y=1+x"
--   case expr of
--     JSOperation [] (Var name) (Op [] value (Reference (Ref refNames [] Nothing Nothing))) ->
--       let
--         recursiveLookup = lookup -- placeholder for generalizing to x.y.z.....
--         fromASTRef = recursiveLookup (head refNames) jsAST
--         prevVarState = recursiveLookup name jsAST
--       in
--         -- if it was a jsOperation really
--         -- will actually be modifyAST really 
--         putAST name (PureOp value (Value fromASTRef))

        -- | FINDING: a chain is pure as well once the initial function is run
        -- | and all the subsequent




        



testEval = do
  x <- eval (JSOperation [] (Just (Let "x")) (Val (Boolean (JSBool True))))
  print $ (\(Boolean (JSBool y)) -> y) x

-- | Control is in the haskell context and expression is in the JS context 

-- | IMPORTANTE!!! All try excepts should be strict for the try block

-- | IMPORTANTE!!! Why dont i have a function runJSFunction :: MonadJS m => ReaderT [ArgName] m ()
  -- | OR!
    -- this takes the AST, adds the args to it, creating a new one, which deletes the args once the
    -- scope is escaped

-- | Run abstraction will have layers
  -- | 1. runControlFlow (topLevel) 
  -- | 2. runOperation (in the eventual case of JSOperation)

-- | eval (general) -> evalCONTROL -> purify Expr >>= evalPureOp { runJSWithCliErr } -> modify (\AST -> ..); continue


-- | IMPORTANTE!!! Variable declarations only escape scope of control statements (except functions,classes)
-- |               when they have 'var <name>' NOT const or let




-- Inner scopes have access to all outer scopes, but the variables CREATED in the context of inner scopes
-- conditionally escape
  -- global keyword (or is that python)?
  -- 'var' from control {}bracket 

-- | Note: the only statement which we actually run is A value or Pure Expression
-- | and therefore, these are the only things we need to console.log() and parse
-- | and therefore, the only thing we need to parse is values
-- |
-- | Consider the edge cases of `return function() {..}` or `return class A{..}`
-- | By our models of running, we will have parsed these into haskell types and then why would we need
-- | to retrieve via console.log? We can understand all JS instructions, just not how JS decides on output
-- | from expressions
-- |
-- | This also suggests that how we should handle return is to set the namespace to the Object
-- | we could make this straightforward by having it be a case statement:
  -- Function -> ...
  -- Object -> ...
  -- Class -> ...
  -- Expression -> ...
-- | So that we may neatly sort it into our tree



testEvalJSOperation1 = Right [JSOperation [] (Just (Let "x")) (Val (Number (JSNumber 1.0)))]

-- | Should realize that its in normal form and run op (for now; later: delay op) 
testEvalJSOperation2 = Right [JSOperation [] (Just (Let "x"))
                              (Op Plus (Val (Number (JSNumber 1.0))) (Val (Number (JSNumber 1.0))))]

testEvalJSOperation3 = Right [JSOperation [] (Just (Let "x"))
                             (Op Plus (Val (Number (JSNumber 1.0)))
                              (Op Plus (Val (Number (JSNumber 1.0)))
                               (Op Plus (Val (Number (JSNumber 1.0)))
                                (Op GreaterThan (Val (Number (JSNumber 1.0))) (Val (Number (JSNumber 1.0))))
                               )))
                            ]

--evalOp :: 


-- | This implies:
data Script' a = Script' [JSTopLevel a] deriving (Show, Eq, Ord)
-- | Which thus implies maybe I should just make the Num a => a, a Float
--- AND
-- | This implies: we could do for space efficiency:
data ScriptRaw = ScriptRaw [(Dependency, RawJS)] deriving (Show, Eq, Ord) -- where type RawJS = JS
-- | AND implies:
newtype ScriptWriter num m a = ScriptWriter { unScriptWriter :: WriterT (Script' num) m a } 
-- runScriptWriter has access to a node path
                
--data Statement = Statement [Dependency] (Maybe Name) JS



--evalReference 

--data JSOperation a = JSOperation [Dependency] (Maybe Name) (Expr a) --JS
eval :: {-MonadJS m =>-} JSOperation a -> IO (JSValue n) -- could also be an Expr
eval (JSOperation deps mName expr) = do
  let
    deRef :: ((Expr a), [Dependency]) -> Expr a
    deRef = undefined

  case mName of
    Nothing -> undefined deRef
      -- just check if any global vars are affected I think, also may be able to just toss
      -- and then return undefined but most likely if this is the context then we are using a
      -- browser API or console.log / some pre-defined object
               -- But i suppose we could also gather this from the deps? 
      
    Just name -> do
      -- | In this case, we will deal with variations of assignment ops
        -- | 2 real cases:
        -- | '=' -> set to result of expression
        -- | x@else -> x <op> exprResult 
      let showJS (Val (Boolean (JSBool True))) = "true" 
      jsValRaw <- liftIO $ runJSWithCliOneExpr $ showJS expr
      print jsValRaw
      let Right jsBool' = parse jsBool "" jsValRaw 
      
      pure . Boolean $ jsBool'
      
      -- we want to parse the log for the name using runJSWithCli
    

-- | Falls back to Nix if it cannot find the path
-- | Compilation will fail if this does 
nodePath :: FilePath
nodePath = $(recover (staticWhichNix "node") (staticWhich "node"))

runNodeJS :: Script -> IO JSVal 
runNodeJS = runJSWithCli 

type DependencyScript = Text
type TargetExpr = Text 
runJS :: [DependencyScript] -> TargetExpr -> IO JSVal 
runJS statements target = runJSWithCli $ Script mempty $ intercalate ";" statements <> ";" <> (clog target)

clog e = ("console.log(" <> e <> ")")

runJSWithCliOneExpr :: Text -> IO JSVal
runJSWithCliOneExpr shownExpr = do
  runJSWithCli (Script mempty $ clog shownExpr)

runJSWithCli :: Script -> IO JSVal 
runJSWithCli (Script _ script) = do
  withTempFile "." "index.js" $ \fp handle -> do
    print $ "fp:" <> fp
    hPutStr handle $ script
    hFlush handle 
    readCreateProcess (proc nodePath [fp]) []


type StdErr = String 
runJSWithCliErr :: Script -> IO (ExitCode, JSVal, StdErr)
runJSWithCliErr (Script _ script) = do
  withTempFile "." "index.js" $ \fp handle -> do
    print $ "fp:" <> fp
    hPutStr handle $ script
    hFlush handle 
    readCreateProcessWithExitCode (proc nodePath [fp]) []


jsStdOutParser :: Stream s m Char => ParsecT s u m (Html, [JSVal]) 
jsStdOutParser = do
  (jsvals, _) <- manyTill_ ((some $ noneOf ['\n']) <* (char '\n')) (string "!@#$%^&*()")
  html <- many anyChar
  pure (html, jsvals)




-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
runVDOMWith :: Text -> Html -> JS -> ExceptT ParseError IO (Html, [JSVal])
runVDOMWith baseUrl indexHtml (JS js) = withTempFile "." "index.html" $ \htmlFPath handle -> do
  liftIO $ hPutStr handle $ pack indexHtml 
  liftIO $ hFlush handle
  liftIO $ print $ baseUrl
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath
  except $ parse jsStdOutParser "" rawJSVal 
  where
    -- mkVDOMScript fp = Script mempty
    --   $  "let fs = require('fs');"
    --   <> "let jsdom = require('jsdom');" 
    --   <> ("const text = fs.readFile(\"" <> (pack fp) <> "\").toString('utf-8');")
    --   <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: true, userAgent: \"Mellblomenator/9000\",});"
    --   <> "const virtualConsole = new jsdom.VirtualConsole();"
    --   <> "virtualConsole.sendTo(console, { omitJSDOMErrors: true });"
    --   <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
    --   <> ", url: \"" <> (pack baseUrl) <> "\", virtualConsole});"
    --   <> js
    --   <> (pack ['\n'])
    --   <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
    --   <> "console.log(dom.serialize());"

    mkVDOMScript fp = Script mempty $ "console.log(" <> js <> ")"
      -- $  "let fs = require('fs');"
      -- <> "let jsdom = require('jsdom');" 
      -- <> ("fs.readFile('" <> (pack fp) <> "', function(err, text) {")
      -- <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: false, userAgent: \"Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0\",});"
      -- <> "const virtualConsole = new jsdom.VirtualConsole();"
      -- <> "virtualConsole.sendTo(console, { omitJSDOMErrors: false });"
      -- <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
      -- <> ", url: \"" <> ("" {-pack . renderLink $ baseUrl-}) <> "\", virtualConsole});"
      -- "console.log(" <> js <> ")"
      --js
      -- <> (pack ['\n'])
      -- <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      -- <> "console.log(dom.serialize());"
      -- <> "});"


-- -- | Scripts can reference the DOM by using the name 'dom' 
-- -- | Could make this a Monad if it is worth it
-- -- | Currently, you need to console.log the return value or this runs but doesnt return
-- runVDOMWithAST :: Html -> JSAST -> [JSInteraction] -> ExceptT ParseError IO (Html, JSAST)
-- runVDOMWithAST indexHtml (JSAST inSet (JS js)) userInput = withTempFile "." "index.html" $ \htmlFPath handle -> do
--   let
--     inSetToSetGet = fmap (\(a,b) -> SetGet a b) $ M.toList inSet
--     mkBoilingPot = inSetToSetGet <> userInput
--     setters = catMaybes $ fmap filterSets mkBoilingPot
--     getters = catMaybes $ fmap filterGets mkBoilingPot
--     -- User input would override auto input
  
--   liftIO $ hPutStr handle $ pack indexHtml 
--   liftIO $ hFlush handle 
--   rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath setters getters
--   (html, mappy) <- except $ parse jsStdOutParserAST "" rawJSVal
-- --  let
-- --     out
--   pure (html, JSAST mappy (JS js)) 
--   where
--     mkVDOMScript fp setIn getOut = Script mempty
--       $  "let fs = require('fs');"
--       <> "let jsdom = require('jsdom');" 
--       <> ("var text = fs.readFileSync(\"" <> (pack fp) <> "\").toString('utf-8');")
--       <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\" });"
--       <> (unJS $ mconcat setIn) 
--       <> (pack ['\n'])
--       <> js
--       <> (pack ['\n'])
--       <> (unJS $ mconcat getOut) 
--       <> (pack ['\n'])
--       <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
--       <> "console.log(dom.serialize());"

  
-- | There's probably an implementation which uses laziness to our advantage
-- | where we are only calling node on functions where we have to 
runVDOM' :: Html -> ExceptT ParseError IO Html
runVDOM' htmlString = fst <$> runVDOMWith ("") htmlString mempty 


-- | There's probably an implementation which uses laziness to our advantage
-- | where we are only calling node on functions where we have to 
runVDOM :: Text -> Html -> ExceptT ParseError IO Html
runVDOM baseUrl htmlString = fst <$> runVDOMWith baseUrl htmlString mempty 


-- GOAL: jsStdOutParserAST :: Stream s m Char => ParsecT s u m (Html, JSAST) 
jsStdOutParserAST :: Stream s m Char => ParsecT s u m (Html, M.Map String String) 
jsStdOutParserAST = do
  (jsvals, _) <- manyTill_ (f <* (char '\n')) (string "!@#$%^&*()")
  html <- many anyChar
  pure (html, M.fromList jsvals)
  where
   --  f = (,) <$> (manyTill alphaNum (try $ string "|>")) <*> (string "|>" >> (some $ noneOf ['\n']))
    f = (,) <$> (manyTill alphaNum (try $ string ":")) <*> (string ":" >> (some $ noneOf ['\n']))


-- | Would grab all variables in top level of JS
-- | and would do so by scraping all top level variable declarations
runGrabJS :: MonadJS m => JS -> m ()
runGrabJS js = undefined
  -- starts with parsing
  -- based on parsing, get relevant vars
  -- run rest 

runJSWithCliStatefully = undefined




mkScriptInterop :: Script -> [JSInteraction] -> Script
mkScriptInterop script interactions = undefined



-- | Stateful runVDOM

runVDOMST :: JS -> JSDomT IO (M.Map Name JSVal)
runVDOMST xs = undefined

        

test2 = do
  let script = Script mempty $ "function f() { return 1; }"
  -- runJSWithCli script
  -- print "withScript"
  -- str <- runJSWithCli $ withScript script (JS "console.log(f() + f())")
  -- print $ "string:" <> str
  str2 <- runJSWithCli $ withScript script (JS "f()")
  print str2
