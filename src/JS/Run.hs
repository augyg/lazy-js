{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


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
import Control.Applicative (some)
import Language.Haskell.TH (recover)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Text (Text, pack, unpack, intercalate)
import System.Which
import System.Process (CreateProcess(..), proc, readCreateProcess, readCreateProcessWithExitCode)
import System.IO (hFlush)
import System.IO.Temp (withTempFile)
import System.Exit (ExitCode)
import Data.Text.IO (hPutStr)

data Link = Link Text -- TODO(galen): delete

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




-- | Inner AST for functions 
type MyAST = JSAST

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
lookupGlobal :: [Name] -> JSAST -> Maybe (ExprAST a)
lookupGlobal = lookupAST 

      
  
  

-- | isJust if full dot pattern was in this AST 
lookupAST :: [Name] -> JSAST -> Maybe (ExprAST a)
lookupAST (n:nChain) (Recordc ast) =
  case Data.List.lookup n ast of
    Just v -> case nChain of
      [] -> Just v
      xs -> case v of
        ValC (RecordC kv) -> lookupAST nChain (RecordC kv)
        _ -> Nothing -- TODO(galen): better error handling 
      lookupAST nChain v
    Nothing -> Nothing
      --error "value referenced before defining"


-- | isJust if full dot pattern was in this AST 
lookupAST :: [Name] -> JSAST -> Maybe ExprAST
lookupAST (n:nChain) (Recordc ast) =
  case Data.List.lookup n ast of
    Just v -> case nChain of
      [] -> Just v
      xs -> case v of
        ValC (RecordC kv) -> lookupAST nChain (RecordC kv)
        _ -> Nothing -- TODO(galen): better error handling 
      lookupAST nChain v
    Nothing -> Nothing
      --error "value referenced before defining"



-- | By this reasoning about types, we can simply 'demand' values by asking for them
-- | for class objects tho, like XMLHTTPRequest, could we 
runMonadJS :: Maybe MyAST -- optional pass of starting AST 
           -> [JSTopLevel a]
           -> m JSAST
runMonadJS = undefined



--data JSAST = JSAST (Map Name ExprAST)
type JSAST a = JSRecordC a 


applyFunc :: (Function a) -> [Expr a] -> Expr a -- evalExpr (ApplyFunc f args) 
applyFunc = undefined



class HasJSState s where
  getRef :: (Fractional a, Read a) => s -> ExprAST a
  -- TODO: this 
  putRef :: (Fractional a, Read a) => s -> Name -> ExprAST a -> ExprAST a
  -- TODO: 
  --modifyRef 
 
  
instance HasJSState (MyAST, [MyAST], JSAST)
instance HasJSState ([MyAST], JSAST)
instance HasJSState JSAST 
  
  


-- Output of smart constructor which will use isUnaffectedByState
data JSValueC a = NumberC (JSNumber a)
                | StringC JSString 
                | NullC JSNull
                | BooleanC JSBool
                | ArrayC (JSArrayC a)
                | RecordC (JSRecordC a)
                | JSUndefinedC 
                
data JSArrayC a = JSArrayC [ExprAST a]
data JSRecordC a = JSRecordC [(Name, ExprAST a)]

-- | Note: when we add key and index lookups, there wont be a direct equivalent ExprAST for that Expr a
-- | Note: this is very similar to what we had for ObjectOriented 
data ExprAST a = ValC (JSValueC a)
               | FuncAST (Function a)
               | ClassAST (Class a)
               | PureOp (ExprAST a) (ExprAST a)



-- | mMyOwnAST ---> inside function
-- | NOTE! This also affects if classes and functions reach global AST
evalTopLevel :: JSTopLevel a -> m (Either (ExprAST a) ())
evalTopLevel mMyOwnAST = \case
  Control' contr -> evalControl mMyOwnAST contr
  Declare oop -> do
    evalDeclare mMyOwnAST oop
    pure $ Right () 
  Return expr -> case mMyOwnAST of
    Nothing -> error "this should never happen: return outside function"
    Just _ -> do
      exprAST <- evalExpr expr 
      pure $ Left exprAST -- Left cuz end with this data 
-------------------------------------
  Break -> undefined -- ? -- i think im gonna do something else 
-------------------------------------          
-- | TODO(galen): var escapes scope of
evalControl :: Control a -> m (Either (ExprAST a) ())
evalControl mMyOwnAST = \case
  While whileLoop -> evalWhileLoop whileLoop
  For forLoop -> evalForLoop forLoop 
  IF ifStatement -> evalIfStatement ifStatement
  Switch switch -> evalSwitch switch
  TryExcept tryExcFin -> evalTryExcept tryExcFin

-- | Do these last: its basically just 'should we run these top levels?'
evalWhileLoop = undefined
evalForLoop = undefined
evalIfStatement = undefined
evalSwitch = undefined
-- | STRICT!!!!!!!!!!!
evalTryExcept = undefined

-- | IF there is no local AST then the global one will be affected
evalDeclare :: MonadJS m => ObjectOriented a -> m () 
evalDeclare myMyOwnAST = \case
  Function' newFunctionDef -> putFunctionAST mMyOwnAST newFunctionDef 
  Class newClassDef -> writeASTWithTemplate mMyOwnAST newClassDef
  Operation operation -> evalJSOp mMyOwnAST operation -- VOILA!




-- | TODO(galen): Can this handle setting a new property ? this.x = 1?     

-- | Eval to WHNF with freezed values from the references
-- | evalExpr should only affect a JSAST through evalOp and never directly itself
-- | however this doesn't mean that if the expression is function app for example, that
-- | it can't modify state
-- | SO: we should no new keys after evalExpr in theory
evalJSOp :: MonadJS m => Maybe MyAST -> JSOperation -> m ()
evalJSOp mMyOwnAST (JSOperation deps varDecl expr) = do

  -- | Handle case of mMyOwnAST 
  
  exprOut <- evalExpr expr
  -- | TEST!
  --when (not $ isUnaffectedByState exprOut) $ error $ "invalid expression handling" <> (show expr) 

 
  case varDecl of
    Nothing -> i think (  evalRef expr ) 
    
      
    --------------------Can only affect innermost---------------------------------------
    -- is like Data.Map.insert
    -- NOTE! All values should be a PureOp or Class or Function
    -- NOTE! classes may be overwritten by values 
    Just (Raw A_Equal ref') -> ""
      -- | Can only affect the innermost function AST (if we are in a fn that is)
      case mMyOwnAST of
        Just myAst -> putSubStateT myAst name objectOriented
        Nothing -> do
          ast <- getAST
          -- | TODO(galen): Can this handle setting a new property ? this.x = 1? 
          putAST ast name objectOriented          
    
    -- Is like Data.Map.adjust :: (a -> a) -> k -> Map k a
    -- | TODO(galen): should disallow change for const (and A_Equal for let)
    -- | TODO(galen): should this use evalRef? 
    Just (Raw iterOp ref') -> do 
      -- | Can update and change global or transitory declarations 
      oo <- getsAST ref' -- TODO(galen): lens for maps
      -- | Note that the next line works regardless of if its actually a Value or Expr
      -- | Although we may need to make the '[Function: name]' and '[class name]' strings  
      putAST $ PureOp $ Op (toAssocOperator iterOp) oo objectOriented
      
    -- is like Data.Map.insert
    -- TODO: should disallow change for prev Const and Let 
    Just notRawName ->
      -- | TODO(galen): it would be valuable to check if Const was used to define this variable we intend to implement
      -- | SUB-TODO: we'd need to handle an error here: what should the business logic be for errors? Configurable?
      let toName = \case
            Let n -> n 
            Var n -> n
            Const n -> n
            _ -> error "wtf" 
      in case mMyOwnAST of
           Just myAst -> putSubStateT myAst name objectOriented 
           Nothing -> do
             ast <- getAST 
             putAST ast name objectOriented
    --------------------Can only affect innermost---------------------------------------      

  
  where
    -- | NOTE: both of these have to be able to handle [Name] (as well as Name) cases
    -- | which implies that we may modify an object (which is really a recursive Map) 
    --    putSubStateT = undefined
    --    putAST = undefined
    toAssocOperator = undefined -- eg: += --> +
    -- | TODO! 
    writeASTWithTemplate = undefined
    --putAST = undefined -- should also make sure to add to MonadJS instance
    -- | TODO(what if the expr is already purified?)
    -- | This should return something that is unaffected by global state; we have reduced this depedency
    -- | on global state to
    -- | TODO(galen): can we confirm that all illogical expressions that are syntacticall valid give JSUndefined?
    evalExpr :: (Fractional, Read a, MonadJS m) => Expr a -> MyAST -> m (ExprAST a)
    evalExpr mMyOwnAST = \case
      Reference r -> evalRef r

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
      New className argExprs -> do
        objC <- createNewObject className argExprs
        pure $ ValC objC
        -- | 1. fold methods into proper nameSpaces (these aren't eval'd yet, but they may be used) 
        -- | 2. call base constructor 
        
        where
          -- data JSClass a = JSClass (Maybe Extends) (Maybe (Constructor a)) [Method a] deriving (Show, Eq, Ord)
          createNewObject' :: JSClass -> [ExprAST a] -> m (JSRecordC a)
          createNewObject' (JSClass mExtends mConstructor methods) args = do
            let super = case mExtends of --if has extends then set super key
                  Just (JSClass mExtends mConstructor methods) -> [("super", constructorToAST mConstructor)] 
                  Nothing -> [] 
            let superMethods = case mExtends of  -- by folding
                  Just (JSClass mExtends mConstructor methods) -> [("super", fmap (nameF . coerceMethodToFunc) methods)]
            
            let
              thisMethods :: [(Name, ExprAST a)]
              thisMethods = fmap (nameF . coerceMethodToFunc) methods
              
            (_, this) <- tryApplyPropertyFunction emptyJSRecord Nothing args
            
          
          nameF :: Function a -> (Name, ExprAST a)
          nameF f@(Function (Just methodName) args tLevs) = (methodName, FuncAST f)


          
          callsSuper :: [JSTopLevel a] -> Bool
          callsSuper = any f
            where
              f :: JSTopLevel a -> Bool
              f = \case
                Control _ -> False
                Return _ -> False
                Break -> False 
                Declare oo -> case oo of
                  Function' _ -> False
                  Class _ -> False
                  Operation (JSOperation _ _ e) -> case e of
                    ApplyFunc (Left "super") _ -> True -- only case lol
                    _ -> False


          



          
          runConstructor :: MonadJS m => Method a -> Maybe Extends -> m (JSRecordC a)
          runConstructor (Method _ argNames tLevels) mExtends = do
            case mExtends of
              Nothing -> ""
              Just parentName -> 
                case callsSuper tLevels of
                  True -> do  -- get super from parent and add this
                    (JSClass mExtends (Method _ argNamesExt tLevelsExt methods)) <- lookupASTs parentName
                    
                  False -> "" -- simple
              


          
          -- runConstructor :: MonadJS m => Method a -> Maybe Extends -> m (JSRecordC a)
          -- runConstructor (Method _ argNames tLevels) = do
          --   case extends of
          --     Just parentName -> do
          --       (JSClass mExtends mConstructor methods) <- lookupASTs parentName
          --       case mConstructor of
          --         Just constructor ->
          --           addArgsToASTScope [ArgName "super"]
                    
          --         Nothing -> pure () 
          --     Nothing -> 
            

          
          -- | NOTE: one fold is the difference between this and super 
            -- | so fold all but the last to get super
            -- | then fold this with the last/immediate class to get 'this'
            
          foldMethods :: [[Method a]] -> Map Name (Method a)
          foldMethods = foldr f mempty 
          
          f :: [Method a] -> [Method a] -> Map Name (Method a)
          f parent child =
            let
              parent' = M.fromList $ nameF <$> parent
              child' = M.fromList $ nameF <$> child
            in
              M.union child' parent' 

        -- if we have an extends: put super as the parent constructor

        --   or the constructor will have a list of top levels, we could simply just replace it with
        --   the super 
        
--         where
--           -- Name results in a list of classes to fold, that can then be run as a function of sorts
--           fakeCase :: JSClass a -> [Expr a] -> m JSRecordC
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

--           createNewObject :: Name -> [Expr a] -> m JSRecordC
--           createNewObject className argExprs = do
--             argsPure <- mapM evalExpr argExprs

--             could we fake this via a function-like interface
--             where we add to an object 'this'

--             let fakeAST = JSRecordC [("this", JSRecordC []), ("super", superConstructor)]
--             let thisToObject (JSRecordC [("this", JSRecordC props):otherNameSpaces]) = JSRecordC props


--             coerceToFunctionReturningThis :: [JSClass a] -> [Expr a] -> ApplyFunc -- (-> JSRecordC)

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
                
                


      
      
      -- | we may need to make the '[Function: name]' and '[class name]' strings if they are these cases
      -- | but in this far, this is valid and we dont need to care 
      Op operator expr1 expr2 -> do
        pureE1 <- evalExpr expr1 
        pureE2 <- evalExpr expr2
        PureOp operator pureE1 pureE2  

      -------------------------------------------------
      --- All of these are ready to be put to global state 
      Val v ->
        val <- case v of
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
          Record obj -> fmap (RecordC . JSRecordC) $ mapM (\(name,e) -> (name,) <$> (evalExpr e)) obj
          JSUndefined -> pure JSUndefinedC 
        pure $ ValC val
      FuncAsExpr f -> pure $ FuncAST f 
      ClassesAsExpr class' -> pure $ ClassAST class'
      -------------------------------------------------
      -- SO WHEN WE ENTER AND EXIT A FUNCTION WE JUST CHANGE THE AST
      -- IN EG.: (Just newASTFromArgs, fromJust lastInner : innerASTs, global)
      -- OUT EG.(from In eg.): (Just $ head innerASTs, tail innerASTs, global) 
      ApplyFunc eithFuncy argExprs -> do
        -- | The value returned will be the Object given to the 'return' keyword
        argsPure <- mapM evalExpr argExprs
        case eithFuncy of
          Right func -> evalFunc func argsPure 
          Left name -> do
            func <- getAST name
            evalFunc mMyOwnAST func argsPure

zipArgs :: MonadJS m => [ArgNames] -> [ExprAST a] -> m [(Name,ExprAST a)]
zipArgs [] _ = pure []
zipArgs (argName:argNames) [] = case argName of
  ArgName _ -> zipArgs argNames []
  ArgDef name expr -> do
    exprC <- evalExpr expr
    liftA2 (:) (name,exprC) (zipArgs argNames [])
zipArgs (argName:as) (exprC:exprCs) =
  let
    toName = \case { ArgName n -> n; ArgDef n _ -> n }
  in
    liftA2 (:) (toName argName, exprC) (zipArgs as bs)


-- By this point we've already eval'd the expr args
addArgsToASTScope :: [ArgName a] -> [ExprAST a] -> m ()
addArgsToASTScope argNames exprs = do
  -- exprCs <- mapM evalExpr exprs
  let newAST = fromList $ zipArgs argNames exprCs
  (mInner, inters, g) <- getAST
  case mInner of
    Just astI -> 
      putAST (Just newAST, astI : inters, g) 
    Nothing ->
      case inters of
        (x:xs) -> error "invalid function-scope state handling" 
        [] -> --logical
          putAST (Just newAST, [], g)
          
          
  
-- Allow infinite nesting without scoping inwards, instead just referring to the same 'this'
addArgsToASTScopeThis :: [ArgName a] -> [ExprAST a] -> m ()
addArgsToASTScopeThis argNames exprs = do
  -- we definitely have super, it just may be empty
  (mInner, inters, g) <- getAST
  let args_@(("this", thisIn):("super": superIn):zipped) = zipArgs argNames exprs
  case mInner of
    Nothing ->
      case inters of
        (x:xs) -> error "invalid handling of state"
        [] -> (fromList args, [], g)

    Just (JSRecordC astI) -> do
      -- let super = lookupAST ["super"] astI
      -- let this = lookupAST ["this"] astI
      let f = \k v -> k == "this" || k == "super"
      let (mSuperThis, astI') = partitionWithKey f astI
      case Map.lookup "this" mSuperThis of
        Nothing -> putAST (Just $ fromList args_, astI: inters, g) 
        Just this' -> do
          -- this already exists, so add to it, prolly same with super
          --partitionWithKey f
          let unionThisSuper = \(ValC (JSRecordC thisOld)) (ValC (JSRecordC thisNew)) -> ValC (JSRecordC (union thisNew thisOld))
          let astInnerNew = (unionWith unionThisSuper mSuperThis (fromList args))
          putAST (astInnerNew, astI':inters, g)
        
      
  
  -- do just like above except check if 'this' entry exists in the astI before shuffling to a higher level
    -- so only shift the astI if 'this' doesnt exist innit

evalFunc :: Function a -> [ExprAST a] -> m (ExprAST a)
evalFunc f@(Function mName argNames tLevels) argExprsPure = do
  addArgsToASTScope argNames argExprsPure --mMyOwnAST
  eithExprAST <- evalFuncInner
  removeDeclarationsFromScope 
  case eithExprAST of
    Right () -> -- never returned 
      pure $ Val JSUndefinedC
    Left returned ->
      pure returned

evalFuncProperty :: Function a -> [ExprAST a] -> m (ExprAST a, JSRecordC a)
evalFuncProperty f@(Function mName argNames tLevels) argExprsPure = do
  addArgsToASTScopeThis argNames argExprsPure --mMyOwnAST
  eithExprAST <- evalFunc'
  this <- getThis 
  removeDeclarationsFromScope 
  case eithExprAST of
    Right () -> -- never returned 
      pure (Val JSUndefinedC, this)
    Left returned ->
      pure (returned, this)
  where
    getThis :: MonadJS m => m ()
    getThis = do
      (mInner, inters, JSObjectC g) <- getAST
      case mInner of
        Nothing -> error "found no inner scope while evaluating function"
        Just astI -> pure . fromJust . (lookupAST ["this"]) $ astI
    
evalFuncInner :: [JSTopLevel a] -> m (Either ObjectOriented ()) 
evalFuncInner (tLevel:tLevels) = do
  eithReturned :: Either (ExprAST a) () <- evalTopLevel tLevel
  case eithReturned of
    Left exprC ->
      pure exprC
    Right () ->
      evalFunc' tLevels

-- | Step out one level of scope 
removeDeclarationsFromScope :: MonadJS m => m ()
removeDeclarationsFromScope = do
  (_, inters, g) <- getAST
  case inters of
    [] -> putAST (Nothing, [], g) -- not in a function now 
    (x:xs) -> putAST (Just x, xs, g) 



-- evalFuncProperty :: Function a -> [ExprAST a] -> m (ExprAST a)
-- evalFuncProperty f@(Function mName argNames tLevels) argExprsPure = do
--   addArgsToASTScope argNames argExprsPure --mMyOwnAST
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
--       (mInner, inters, JSObjectC g) <- getAST
--       case mInner of
--         Nothing -> error "found no inner scope while evaluating function"
--         Just astI -> pure . fromJust . (lookupAST ["this"]) $ astI
    
--     removeDeclarationsFromScope :: MonadJS m => m ()
--     removeDeclarationsFromScope = do
--       (mInner, inters, JSObjectC g) <- getAST 
--       case mInner of
--         Nothing -> error "removal of non-existent AST upon completion of funcProp"
--         Just astI ->
--           let this = lookupAST ["this"] astI
--               -- super = lookupAST ["super"] astI
--           in case inters of
--                [] -> putASTs (Nothing, [], JSRecordC (
          
      
--     evalFunc' :: [JSTopLevel a] -> m (Either ObjectOriented ()) 
--     evalFunc' (tLevel:tLevels) = do
--       eithReturned :: Either (ExprAST a) () <- evalTopLevel tLevel
--       case eithReturned of
--         Left exprC ->
--           pure exprC
--         Right () ->
--           evalFunc' tLevels


handleThis


so while it is true that we could have a this reference which refers to two possible cases,
we can deterministically rule out the possibility of it being the inner AST if

A) mInner == Nothing
B) Just astI >>= lookupAST ["this"] == Nothing


Also we cannot surprisingly enter nested functions

eg:
  iter() is a method

some other method cannot do:

  f() { x = iter } ; it must do:  f() { x = this.iter } 


Although how would double nested 'this' work?


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

type NameChain = [Name]
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
      Just match -> pure $ Just match
      Nothing -> case getFirst $ First . (lookupAST (n:nameChain)) <$> inters of
        Just match' -> pure $ Just match'
        Nothing -> pure $ lookupAST nameChain global 


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
evalRef :: Ref -> ExprAST a
evalRef r@(Ref nameChain deps mArgs mChain) = do
  -- TODO: refactor
  asts@(mInnerScope, interScopes, global) <- getAST

  exprC <- lookupASTs nameChain
  handleFoundLookup nameChain exprC mArgs 
  
  where 
    
    --handleFoundLookup :: Ref -> ExprAST a -> m (ExprAST a)
    handleFoundLookup :: MonadJS m =>
                         [Name]
                      -> ExprAST a 
                      -> Maybe [Expr a]
                      -> Maybe (Ref a)
                      -> m (ExprAST a)
    handleFoundLookup nameChain exprC args mChain = -- (Ref nameChain _ mArgs mChain) exprC = 
      case mArgs of
        Nothing -> pure exprC  
        Just args -> do
          let allButLast = Data.list.init nameChain
          objectFrom <- lookupASTs allButLast
          case exprC of
            FuncAST f -> 
              (returned, this') <- tryApplyPropertyFunction objectFrom f args
              putAST allButLast this'
              chainExprMaybe returned mChain

            ClassAST _ -> erroridcclass
            _ -> error "type error: applied args to non function"
          
    -- factoring out helper
    tryApplyPropertyFunction :: MonadJS m =>
                                JSRecordC a
                             -> Function a 
                             -> [Expr a]
                             -> m (ExprAST a, JSRecordC a)
    tryApplyPropertyFunction objectFrom f args = do 
      -- | TODO(galen): What about when a propertyFunction calls another propertyFunction
      -- | Then we don't change/nest in this and super otherwise we'll have
      -- | this.this.x
      let f' = (Function mName ((ArgName "this") : (ArgName "super") :argNames) tLevels)
      let super = fromMaybe (JSRecordC []) $ lookupAST "super" objectFrom 
      let args' = (ValC (RecordC objectFrom)) : ( super ) : argsPassed
      evalFuncProperty f' args'

    -- | we may need to run node in this case and with String
    -- (function f() {}).name -> 'f'
    chainExprMaybe :: ExprAST a -> Maybe (Ref a) -> m (ExprAST a)
    chainExprMaybe exprC = \case 
      Nothing -> pure exprC
      Just (chainRef :: Ref) -> case exprC of
        ValC (RecordC jsRecordC') -> evalChainedRef jsRecordC' chainRef
        Valc (StringC s) -> error "unimplemented" -- "".length()
        FuncAST f -> error "unimplemented"
        _ -> error "type error: applied chain to unchainable type"
    
    evalChainedRef :: JSRecordC a -> Ref -> m (ExprAST a)
    evalChainedRef jsRecordC (Ref nameChain deps mArgs mChain) = do
      case lookupAST nameChain jsRecordC of
        Nothing -> error "chain on non-existent property"
        Just exprAST ->
--          handleFoundLookup nameChain 
          case mArgs of
            Nothing -> pure exprAST
            Just argsPassed -> do
              -- is impossible for 'this' here to affect global state
              -- at least on this Statement; we could set a variable equal to the returned value
              -- then it would have capacity to affect the ASTs  
              (returned, _) <- tryApplyPropertyFunction nameChain mChain exprAST argsPassed
              chainExprMaybe returned mChain 



      
      
      
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
  
evalScript :: MonadJS m => [JSTopLevel a] -> m JSAST
evalScript = mapM_ evalTopLevel 


type DepExpr a = (Expr a, [Dependency])


-- Should we ever copy AST for a massive deRef? In case that never needs to happen?
  -- I mean, it would still be lazy `\_o_/`
  -- like an array which parses to be quite intensive (eg. 10000s of refs) 

data JSASTValue a = Pure a
                  | Object (ObjectOriented a) 

deRefExample = do
  let jsAST = [("x", "10")] 
  
  let Right (expr:[]) = parse jsOperation "" "y=1+x"
  case expr of
    JSOperation [] (Var name) (Op [] value (Reference (Ref refNames [] Nothing Nothing))) ->
      let
        recursiveLookup = lookup -- placeholder for generalizing to x.y.z.....
        fromASTRef = recursiveLookup (head refNames) jsAST
        prevVarState = recursiveLookup name jsAST
      in
        -- if it was a jsOperation really
        -- will actually be modifyAST really 
        putAST name (PureOp value (Value fromASTRef))

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

data JSPure a = NormalForm (JSValue a)
              | PureOp Operator (JSPure a) (JSPure a)

toPure :: (Expr a, [Dependency]) -> m (JSPure a )
toPure = undefined

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


-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
runVDOMWithAST :: Html -> JSAST -> [JSInteraction] -> ExceptT ParseError IO (Html, JSAST)
runVDOMWithAST indexHtml (JSAST inSet (JS js)) userInput = withTempFile "." "index.html" $ \htmlFPath handle -> do
  let
    inSetToSetGet = fmap (\(a,b) -> SetGet a b) $ M.toList inSet
    mkBoilingPot = inSetToSetGet <> userInput
    setters = catMaybes $ fmap filterSets mkBoilingPot
    getters = catMaybes $ fmap filterGets mkBoilingPot
    -- User input would override auto input
  
  liftIO $ hPutStr handle $ pack indexHtml 
  liftIO $ hFlush handle 
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath setters getters
  (html, mappy) <- except $ parse jsStdOutParserAST "" rawJSVal
--  let
--     out
  pure (html, JSAST mappy (JS js)) 
  where
    mkVDOMScript fp setIn getOut = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
      <> ("var text = fs.readFileSync(\"" <> (pack fp) <> "\").toString('utf-8');")
      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\" });"
      <> (unJS $ mconcat setIn) 
      <> (pack ['\n'])
      <> js
      <> (pack ['\n'])
      <> (unJS $ mconcat getOut) 
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"

  
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
