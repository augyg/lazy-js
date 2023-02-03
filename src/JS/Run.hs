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

todo
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
data ExprAST a = ValC (JSValueC a)
               | FuncAST (Function a)
               | ClassAST (Class a)
               | PureOp (ExprAST a) (ExprAST a)
                 

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
  when (not $ isUnaffectedByState exprOut) $ error $ "invalid expression handling" <> (show expr) 

 
  case varDecl of
    Nothing -> pure ()
    -- | TODO(galen): detect if we care about this effect
      
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
          putAST ast name objectOriented          
    
    -- Is like Data.Map.adjust :: (a -> a) -> k -> Map k a
    -- TODO(galen): should disallow change 
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
    putSubStateT = undefined
    putAST = undefined
    toAssocOperator = undefined -- eg: += --> + 

    -- | TODO(what if the expr is already purified?)
    -- | This should return something that is unaffected by global state; we have reduced this depedency
    -- | on global state to
    -- | TODO(galen): can we confirm that all illogical expressions that are syntacticall valid give JSUndefined?
    evalExpr :: (Fractional, Read a, MonadJS m) => Expr a -> MyAST -> m (ExprAST a)
    evalExpr mMyOwnAST = \case

      Reference ref -> evalRef myMyOwnAST ref
      -- | we may need to make the '[Function: name]' and '[class name]' strings if they are these cases
      -- | but in this far, this is valid and we dont need to care 
      Op operator expr1 expr2 -> do
        pureE1 <- evalExpr expr1 
        pureE2 <- evalExpr expr2
        PureOp pureE1 pureE2  

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
          -- where
          --   processKeyValue :: (Name, Expr a) -> m (Name, ExprAST a)
          --   processKeyValue (name, expr) = do
          --     exprC <- evalExpr expr
          --     pure (name, exprC) 
          JSUndefined -> pure JSUndefinedC 

        pure $ ValC val
          

      FuncAsExpr f -> putFunctionAST varDecl f 
      ClassesAsExpr class' -> putClassAST varDecl class'
      -------------------------------------------------
      ApplyFunc eithFuncy argExprs -> do
        -- | The value returned will be the Object given to the 'return' keyword
        argsPure <- mapM evalExpr argExprs
        case eithFuncy of
          Right func -> evalFunc mMyOwnAST func argsPure 
          Left name -> do
            func <- getAST name
            evalFunc mMyOwnAST func argsPure 
      New name argExprs -> do
        argsPure <- mapM evalExpr argExprs
        writeASTWithTemplate varDecl name argsExpr 

    -- | This could also be used with VarDecl ~ Raw Op [Name], at least for getsAST
    -- | ALL AST Directly
    -- | How to handle if we are in a function?
    evalRef :: ([MyAST] -- innerMost are on top of stack (first) 
               , JSAST)
            -> Ref
            -> ObjectOriented
    evalRef asts (Ref nameChain deps mArgs mChain) = do
      lookupAST nameChain asts 
      
      
      
      -- | best way to set this up is to allow infinite recursion (and therefore also simple lookups)
      -- | and then check if its a func; if it is, then apply args if not (== [])
      -- | then if we have args for a func: check if we chain (we can now assume the return-ed is an Object)
         -- | DoChain --> pass return statement to chain statement
               -- | Only ns@[Name] ~~ Property Grab from (Return x) --> \x -> evalRefObj ns x
               -- | [Name] <+> FnCall ~~ Property Grab from (Return x) --> \x -> if isFn then evalFunc x args 
                    -- | Except since this may infinitely repeat, call evalRef which will call evalFunc
      
    putFunctionAST = undefined
    putClassAST = undefined
    writeASTWithTemplate = undefined

    -- | How to handle if we are in a function?
    -- | TODO(galen) : runStateT evalFunc' ((mkAST argNames argsPure) : containerAST)
    evalFunc f@(Function mName argNames tLevels) argsPure = do
      -- | WAIT! how do we know that argsPure is actually pure???????????
      ast <- getAST
      -- | But we can also be in a nested function which needs access to the transitory AST 
      
      let ast' = (assign argNames argsPure) -- length argsIn /= length argsTaken
      evalFunc' ast tLevels
      where
        -- | SHOULD ALWAYS CHECK LOCAL ASTs first (maybe able to merge them) 
        evalFunc' myOwnAST (topLevel:topLevels) = do
          -- the only thing that should return a value is a return statement which would imply
          -- we are done this loop (we may end before eval-ing all topLevels in a func)
          evalTopLevel (Just myOwnAST) topLevel >>= \case
            Right pureValue -> pure pureValue
            Left myOwnAST -> evalFunc' myOwn
    
    -- | mMyOwnAST ---> inside function
    -- | NOTE! This also affects if classes and functions reach global AST 
    evalTopLevel mMyOwnAST = \case
      Control' contr -> evalControl mMyOwnAST contr
      Declare oop -> evalDeclare mMyOwnAST oop
      Return oop -> case mMyOwnAST of
        Nothing -> error "this should never happen: return outside function"
        Just _ -> pure $ Right oop
  -------------------------------------
      Break -> undefined -- ? -- i think im gonna do something else 
  -------------------------------------          
    -- | TODO(galen): var escapes scope of 
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
    evalDeclare myMyOwnAST = \case
      Function' newFunctionDef -> putFunctionAST mMyOwnAST newFunctionDef 
      Class newClassDef -> writeASTWithTemplate mMyOwnAST newClassDef
      Operation operation -> evalJSOp mMyOwnAST operation -- VOILA!
        




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
