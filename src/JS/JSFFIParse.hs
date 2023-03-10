{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-} 

{-|
Known Exceptions:

Does not handle Object.*(someObj, js) cases (of Type Update)

|-}

module JS.JSFFIParse where

import JS.Types
import JS.Source
import JS.MonadJS

import Control.Applicative (some, liftA2, empty )
import Text.Parsec
import Data.Monoid (All(..))
import Data.These
import Data.Map (Map)
import Data.Text (Text)
import Data.List (intercalate)

-- faked, not really in use yet
import Control.Monad.Trans.Writer 


todo = undefined


{-
TODOS:

https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar

-- Investigate possibility of just getting Firefox JavaScript API source code
   -- and just processing like any other source code 

-- | Break
"The break statement breaks out of a switch or a loop."  break <OPTIONAL:labelname>;
list: {
  text += cars[0] + "<br>";
  text += cars[1] + "<br>";
  text += cars[2] + "<br>";
  break list;
  text += cars[3] + "<br>";
}
-- |

v-- | TODO(galen): Condition comes packaged with the [Dependency]
-- TODO(galen): OR this can just be a function and anonymous function: var x = function(x){}
-- | TODO(galen): iterators
-- | TODO(galen): obj.newField = <VAL>
--  todo: assignment operators
-- TODO multiple vars per var keyword

-- Spacing for expressions

-- TopLevels and operations ending with  (\n | ' ') 

-- Simplify JSValue, Object(re-name to ObjectOriented), maybe control
  -- + all nums are just Float

-- Condition carries deps as well 

-- Array, Object

-- Differentiate between const, var, let (see in JS.Run)

-- Validate that return can be in switch inside fn

-- Throw keyword (other keywords?)
-- TODO(galen): add throw as an expression (or maybe operation) option

-- Run module gets err from stderr for use in catch block 

-- if statements can instead just be followed by throw:
   --> eg: if(x < 5) throw "is too low";
   --> EDIT: by any keyword

-- Comments
  -- // , block /* */(unaffected by newlines)
  -- #!/usr/bin/env node

-- Labels:
  --> lbl: console.log(1); // Label

-- Scientific numbers, Numeric separators

-- Should I do "fuck it" JSOperation case where I just run it as a raw string? cuz i cant interpret it
   -- could set up logging system to write the failed case
   -- if 99% of the cases are handled then so what 


-- Make and use fn:
  scopeBody = between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope

-- | (Regarding Objects)
-- | TODO(galen): NOTE: for a: x += 1 or  a: x = 1  console.log (<obj>.a) --> x+1 or 1

-- There is notably a lot of places that take an ObjectOriented except not an operation with Let Var Const
  -- re-jig this

-- Better way to deal with function naming (eg method)

-- JSExpression to define a class

-- CASE: (function f() { return "dedwed" })().length --> 6
  -- is like a mix of applyFunc and chain
  -- Solution: can chain on any expression (except for Number|undefined|Bool: JSValue = { Set:Object} | { Set:Obj } })


-- Ternary OP: x ? x : y
  -- should be its own Expr case 


(while | if | for ) all can be followed by single statement that doesnt use let const or var
   --> No functions either EXCEPT anon funcs
   --> would still technically be [JSTopLevel a] 
   

-}


reservedKeywordsTODO = [ "break"
                       , "return"
                       , "goto"
                       , "continue"
                       , "throw"
                       , "export" -- ?
                       , "delete"
                       -- On experimentation: this only works for classes not objects
                       , {-classes-} mconcat ["instanceof","extends", "this", "static","implements"
                                             ]
                       -- 
                       , "import"
                       -- AND
                       , "from" -- import from 
                       ---
                       , "in" -- + for .. in ...
                       , "of"
                       , "typeof" -- typeof 1 | typeof(1)
                       , "void"
                       , "async"
                       , "arguments" -- arguments[n] gets nth arg of calling functionx
                       ]


-- | NOTE: return, break, throw, all end their respective control flows
  -- | maybe this is a pattern worth noticing and building around somehow


-- do
--   letJS name value
--   withJS [name1, name2] $ \(x:x2) -> 



sspace :: Stream s m Char => ParsecT s u m [Char]
sspace = many (char ' ') 


inSpace :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inSpace p = sspace *> p <* sspace


testVarString = "var ue_id = 'WEBRWTDQ8PSAD8KS77XA'"

-- test1 = parse (jsVarName >> jsString ) "" testVarString
-- test3 = parse (jsVarName >> jsBool ) "" "var ue_navtiming = true"
-- test4 = parse (jsVarName >> jsNull ) "" "var ue_navtiming = null"
between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  open
  (x, _) <- manyTill_ inside close
  pure x

jsString :: Stream s m Char => ParsecT s u m JSString'
jsString = do
  str <- (try $ between' (char '\'') (char '\'') anyChar) <|> (between' (char '\"') (char '\"') anyChar) 
  pure $ JSString str

-- | TODO(galen): parse as scientific 
jsNumber :: (Num a, Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSNumber a)
jsNumber = do
  whole <- some digit
  decii <- option "" $ do
    dot <- char '.'
    decimal <- some digit
    pure (dot : decimal) 

  pure . JSNumber $ read (whole <> decii)



jsBool :: Stream s m Char => ParsecT s u m JSBool
jsBool = JSBool <$> ((True <$ string "true" ) <|> (False <$ string "false"))

jsNull :: Stream s m Char => ParsecT s u m JSNull
jsNull = JSNull <$ string "null"

manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go

jsValue :: (Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSValue a)
jsValue = 
  (Number <$> (try jsNumber))
  <|> (String' <$> jsString)
  <|> (Null <$> (try jsNull))
  <|> (Boolean <$> (try jsBool))
  <|> (Array <$> (try jsArray)) -- NOTE: this can be lazy cuz the parser will be lazy 
  <|> (Record <$> (try jsonjsObject))
  <|> (JSUndefined <$ (try $ string "undefined"))
  <|> (NaN <$ (try $ string "NaN"))
  <|> (Infinity <$ (try $ string "Infinity"))




jsArray :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (JSArray a)
jsArray = do
  fmap (JSArray . (fmap fst)) $ between1 (char '[') (char ']') $ sepBy jsExpression (char ',')




-- class A { someMethod() { return 1 } } --> Object :: (with x = new A()): { someMethod : function() { return 1 }

-- x.someMethod :: Function

-- but may also be

-- x.someMethod :: Value

-- So therefore, this is an Expr

-- so when an object is declared, it must read refd variables from state and evaluate

-- definitions do not read refs until they are actually called 

-- New is really just a function 
-- function new(classDef, argsForConstructor) {
--   set object.<method> for method in classDef
--   set object.props
--   object = classDef.constructor(argsForConstructor, objectWithMethodRefs) --runConstructor 
--     return object 
--   }

-- newObjectOfClass = do
--   object <- makeEmptyObject 
--   declareMethods `in'` object 
--   object' <- runConstructor object


--   ... in JSOperation (assuming is named) 
--   putAST object' 


jsonjsObject :: (Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSObject a)
jsonjsObject = do
  -- effectively json with functions
  fmap JSObject $ between1 (char '{') (char '}') $ sepBy (try keyAndValue <|> (coerceMethod <$> method)) (inSpace $ string ",")
  where

    keyAndValue :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Name, Expr a)
    keyAndValue = do
      n <- jsValidName
      inSpace $ string ":"
      expr <- jsExpression -- but this can also be more : class and function
      pure (n,fst expr) -- cuz we will be deprecating Deps 

    -- | A method is a special function that can be used in objects and object templates (classes)
        -- Via an alternative way to define the function control flow 
    coerceMethod :: Method a -> (Name, Expr a)
    coerceMethod (Method n args stmts) = (n, FuncAsExpr $ Function (Just n) args stmts)
    
    method :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Method a)
    method = do
      n <- jsValidName
      tup <- jsArgTuple
      stmts <- between1 (char '{') (char '}') $ allowedStatementControlMany True
      pure $ Method n tup stmts

--   One way we could make this clean is to coerce a raw exper
--   then this is the category of Object 

-- Operation Val 

-- oneExpr :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Expr a)
-- oneExpr 
  

-- oneObject :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (ObjectOriented a)
-- oneObject = do
--   (Function' <$> (try jsFunction))
--   <|> (Class <$> (try jsClass))
--   <|> (Operation <$> jsOperation')

--   where jsOperation' = do
--           o <- jsOperation
--           case o of
--             [] -> parserFail "this should never happen"
--             x:[] -> pure x
--             (x:y:xs) -> parserFail "not accepted here"
-- --singularRawExpression 



                     

between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a 
between1 open end match = open *> match <* end
    
jsArgTupleInput :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m [(Expr a, [Dependency])]
jsArgTupleInput = do
  char '(' *> sepBy jsExpression (char ',') <* char ')'

jsArgTuple :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m [ArgName a]
jsArgTuple = do
  char '(' *> sepBy (jsArgName <|> withDefaultArg) (char ',') <* char ')'
  where
    withDefaultArg = do
      name <- jsValidName
      manyTill_ (char ' ') (char '=')
      many (char ' ')
      v <- jsExpression
      pure $ ArgDef name $ fst v -- cuz we are deprecating Deps 

    jsArgName = ArgName <$> jsValidName

-- | All the control flows which take a Bool should be set to False 
jsTopLevelStatement :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (JSTopLevel a)
jsTopLevelStatement = 
  (Control' <$> (control False))
  <|> (Declare <$> (Function' <$> (try jsFunction) <|> (Class <$> (try jsClass))))
  where
    control :: (Fractional a, Read a, Stream s m Char) => Bool -> ParsecT s u m (Control a)
    control inFnScope =
      (While <$> (try $ whileLoop inFnScope))
      <|> (For <$> (try $ forLoop inFnScope))
      <|> (Switch <$> (try $ switchStatement inFnScope))
      <|> (TryExcept <$> (try $ tryExceptFinally inFnScope))
      <|> (IF <$> (try $ ifStatement inFnScope) )

jsClass :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (JSClass a)
jsClass = do
  -- Verified: methods will share state
    -- I guess considering that there are such things as globals and that we have a system for dealing with
    -- them, this is something we can easily handle with a trick

  -- (this.propertyA | class=MyClass )--> VARNAME : MyClass_propertyA
  -- OR we can just store as object: a new type

  inSpace (string "class")
  mClassName <- optionMaybe $ jsValidName <* sspace
  mExtends <- optionMaybe $ string "extends" *> sspace *> (inSpace jsValidName)
  char '{'
  mConstr <- optionMaybe constructor
  methods <- many classMethod
  --many methodWithMaybeClassPrefixes
  char '}'

  pure $ JSClass mClassName mExtends mConstr methods
  where
    constructor = do
      string "constructor"
      args <- inSpace jsArgTuple
      stmts <- between1 (char '{') (char '}') $ allowedStatementControlMany True
      pure $ Method "constructor" args stmts

    -- | TODO(add prefixes)
    classMethod = do
      _ <- prefix -- TODO research these , they also affect names and maybe control flow
      n <- jsValidName
      tup <- jsArgTuple
      stmts <- between1 (char '{') (char '}') $ allowedStatementControlMany True
      pure $ Method n tup stmts


    prefix = todo 
    -- methodWithMaybeClassPrefixes = do
    --   optionMaybe 
data Prefix 

returnStatement :: Stream s m Char => ParsecT s u m (JSTopLevel a)
returnStatement = undefined

-- | This is only because a JSOperation parser can yield many Operations in one constructual line
allowedStatementControlMany :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m [JSTopLevel a]
allowedStatementControlMany inFnScope = mconcat <$> (many $ try $ allowedStatementControl' inFnScope)
allowedStatementControl' :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m [JSTopLevel a]
allowedStatementControl' inFnScope =  
  (toList' $ try mReturnStatement)
  <|> (toList' $ Declare <$> (Function' <$> (try jsFunction) <|> (Class <$> (try jsClass))))
  <|> (toList' $ Control' <$> (control inFnScope))
  <|> ((fmap (Declare . Operation)) <$> jsOperation)
  where
    toList' :: ParsecT s u m a -> ParsecT s u m [a]
    toList' = fmap (:[])
    
    mReturnStatement = if inFnScope
                       then (returnStatement)
                       else empty

    control :: (Fractional a, Read a, Stream s m Char) => Bool -> ParsecT s u m (Control a)
    control inFnScope =
      (While <$> (try $ whileLoop inFnScope))
      <|> (For <$> (try $ forLoop inFnScope))
      <|> (Switch <$> (try $ switchStatement inFnScope))
      <|> (TryExcept <$> (try $ tryExceptFinally inFnScope))
      <|> (IF <$> (try $ ifStatement inFnScope) )

    -- jsFunction = undefined -- for now
    -- jsClass = undefined

-- for (let i = 0; i < 5; i++) {
--     console.log(i)
-- }
forLoop :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (ForLoop a)
forLoop inFnScope = do
  triplet <- forHead
  topLevels <- between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope
  -- NOTE: the topLevels may contain the iterator and the control var may be set before the loop
  pure $ ForLoop triplet topLevels 
  where
    forHead = do
      inSpace $ string "for" 
      inSpace $ char '('
      mControlVariable <- optionMaybe jsOperation
      inSpace $ char ';'
      exitCondition <- jsExpression
      inSpace $ char ';'
      mIterator <- optionMaybe jsOperation  -- probably gonna opt not to pass anyways 
      inSpace $ char ')'
      pure (mControlVariable, exitCondition, mIterator) 
    
    forIn = undefined -- for (key in object) -- Specifically object 
    forOf = undefined -- for (variable of iterable) -- Specifically iterables
  
 
-- | Syntax
-- if (condition1) {
--   //  block of code to be executed if condition1 is true
-- } else if (condition2) {
--   //  block of code to be executed if the condition1 is false and condition2 is true
-- } else {
--   //  block of code to be executed if the condition1 is false and condition2 is false
-- }
-- | Control is in the haskell context and expression is in the JS context

-- | TODO(galen): Condition comes packaged with the [Dependency]
-- in order
ifStatement :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (IFStatement a)
ifStatement inFnScope = do
  if' <- ifClause
  elseifs <- many $ try elseif
  else' <- option [] $ (:[]) <$> maybeElse

  pure $ IFStatement $ if' : elseifs <> else'
  -- Else is not labelled specially since we can just assume it's true
  where
    expression = inSpace (char '(')  *> jsExpression <* inSpace (char ')')
    block = between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope
    
    ifClause = do
      inSpace $ string "if" 
      e <- expression
      topLevels <- block
      pure (e, topLevels)

    elseif = do
      inSpace $ string "else if"
      e <- expression
      topLevels <- block
      pure (e, topLevels)

    maybeElse = do
      inSpace $ string "else"
      (((Val (Boolean (JSBool True))),[]),) <$> block
    
-- | Syntax  
-- switch(expression) {
--   case x:
--     // code block
--     break;
--   case y:
--     // code block
--     break;
--   default:
--     // code block
-- }

switchStatement :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (SwitchStatement a)
switchStatement inFnScope = do
  -- blocks must end with break EXCEPT For default
  -- May not even give a fuck about 'break' here, just toss it, and make Break specifically for loop control
  inSpace $ string "switch"
  expr <- inSpace $ between1 (char '(') (char ')') jsExpression
  char '{'
  cases <- many $ try caseBlocks
  mDef <- optionMaybe default' 
  char '}'

  pure $ SwitchStatement expr cases mDef
  where
    caseBlocks = do
      v <- inSpace (string "case") *> jsValue <* char ':'
      optional (char '\n')
      tlstmts <- allowedStatementControlMany inFnScope
      pure (v, tlstmts)  

    --default' :: Stream s m Char => ParsecT s u m [JSTopLevel a]
    default' = do
      inSpace (string "default") >> char ':'
      optional (char '\n')
      tlstmts <- allowedStatementControlMany inFnScope
      pure tlstmts
      -- and there's essentially an optional newline char 

-- | I guess when I do runSwitchStatement I should just check for Break statements 

-- | Combinator for expecting at least one of two things next 
parseThese :: Stream s m Char => ParsecT s u m this -> ParsecT s u m that -> ParsecT s u m (These this that)
parseThese this that = do
  mThis <- optionMaybe this
  mThat <- optionMaybe that
  case mThis of
    Just this -> case mThat of
      Just that -> pure $ These this that
      Nothing -> pure $ This this 
    Nothing -> case mThat of
      Just likeThat -> pure $ That likeThat
      Nothing -> parserFail "neither found of These" 
  
tryExceptFinally :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (TryExceptFinally a)
tryExceptFinally inFnScope = do
  -- Seems that you must have at least one of the two of catch or finally and try is demanded
  
  TryExceptFinally <$> tryBlock <*> (parseThese (try catchBlock) (try finallyBlock))
  where
    -- TODO(galen): add throw as an expression (or maybe operation) option 
    
    tryBlock = do
      inSpace $ string "try"
      between1 (char '{') (char '}') (allowedStatementControlMany inFnScope) <* (optional $ char '\n')
      
    -- | TODO(multiple different error types)
    catchBlock = do
      inSpace $ string "catch"
      mDep <- optionMaybe $ between1 (char '(') (char ')') jsValidName 
      stmts <- between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope
      pure (mDep, stmts)

    finallyBlock = do
      inSpace $ string "finally"
      between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope 
      
      

--whileLoop inFnScope = undefined


whileLoop :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (WhileLoop a) 
whileLoop inFnScope = do
  (try normal) <|> doWhile
  where    
    normal = do
      string "while"
      headExpr <- between1 (char '(') (char ')') jsExpression
      statements <- between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope
      pure $ WhileLoop headExpr statements
    
    doWhile = do
      string "do"
      statements <- between1 (char '{') (char '}') $ allowedStatementControlMany inFnScope
      optional $ char '\n'
      string "while"
      many (char ' ')
      headExpr <- between1 (char '(') (char ')') jsExpression
      pure $ WhileLoop headExpr statements

-- | 
-- Function
-- Bracket <+> Function <+> Bracket <+> ArgTuple 
-- data Function a = Function (Maybe Name) [ArgName a] [Dependency] [Statement]
-- ( __f__ <|> __fT2__ <|> fT3 )(tuple)
--NOTE a function doesn't need all arguments to run and if it cannot compute the return value -> undefined
jsFunction :: (Read a, Fractional a, Stream s m Char) => ParsecT s u m (Function a)
jsFunction = do
  -- In general, I dont think this should call jsTopLevel but rather some (maybe all) of the same parsers
  -- that JS top level would and add a case via eitherP that allows matching on a return statement
  -- that is treated as a special case only for this parser
  -- NOTE: for now it's fine that most parsers will be undefined such as loops
  --
  -- NOTE: This could also define a function inside it, which has it's own returnStatement (nesting) 
  (try normalFunc) <|> anonArrow
  where
    returnStatement = undefined
    fnLoops = undefined -- validloopStatementTopLevels <|> returnStatement
    fnCaseStatement = undefined -- valdCaseStatementTopLevels <|> returnStatement 
    
    anonArrow = do
      args <- inSpace $ (((:[]) . (ArgName)) <$> jsValidName) <|> jsArgTuple
      inSpace $ string "=>"
      body <- (between1 (char '{') (char '}') $ allowedStatementControlMany True) 
              <|> (((:[]) . Declare . Operation . (\(e,deps) -> JSOperation deps Nothing e)) <$> jsExpression)
      pure $ Function Nothing args body
      
      
    
    normalFunc = do
      (fnName, args) <- functionHead
      bodyStatements <- functionBody args
      pure $ Function fnName args bodyStatements
      where
        functionHead = do
          string "function"
          mFnName <- optionMaybe jsValidName
          args <- jsArgTuple
          pure (mFnName, args) 

        functionBody headArgs = do
          between1 (char '{') (char '}') $ allowedStatementControlMany True
            



-- | FOR expression: 
  -- -- Anything which can be named 
  -- data Expr a = Val (JSValue a)
  --             | Reference [Ref' a] -- decl. function app and/or object reference
  --             | Op Operator (Expr a) (Expr a) --(RefChain a)
  --             | AnonFunc (Function a)
  --             | ApplyFunc (Function a) [Expr a]
  --             | New Name [Expr a]
  --             -- var a = function f(x) { ... }
  --             -- note that f is not defined 
  --
  -- | Does the jsFunction case suggest that for a given JSOperation, in the case of functions and refs in
  -- | general that we must "walk" the function to a value?
    -- | So Op Operator (JSValue a) (JSValue a)
    -- |
    -- | because we don't know what effects on globals or side effects exist in the function calls
  -- |
  -- | so then we need toPure :: Expr a -> Pure a ; an expression with only logical or mathematical operations
  --
  -- | ISSUE #2
    -- | how do we handle new?
    -- | I believe I can do a similar thing: convert to an object which has all of the proper functions and
    -- | properties, including those from super
    -- |
    -- | Also important to note that some classes have functions that can be called before initialization
    -- | This would cause a problem if the script ask `x instanceof <Class>`
    -- | EDIT: we can do this by setting a static property
      -- | eg: x.constructor.name
      -- | and also therfore x.constructor 
    


-- | 
-- anonFunc = do
--   arrowSyntax
--   <|> functionWithName
--   <|> functionWithOutName
--   <|> if with brackets, may be followed by an arg tuple 






-- data Object a = Function' (Function a)
--               | Class {-todo:make-}
--               | Operation (JSOperation a)
              
-- data Control a = While (WhileLoop a)
--                | For {-todo:(ForLoop a) -}
--                | IF
--                | Switch
--                | TryExcept 
--                -- | Return 

-- data JSTopLevel a = Control' (Control a)
--                   | Object' (Object a)
--                   | Return (Object a)


-- grabDeps :: [JSTopLevel a] -> [Dependency]
-- grabDeps = 


-- | ******
-- | NOTE that while we do gather all deps, we could still in theory, step through a loop
-- | or perform partial evaluation although I'm not sure that would be super useful
-- | Although this may be useful for a conditional (including try except) 

-- | Regarding above: (******)
-- | We could definitely customize how we evaluate non-loops
-- | (i mean we could run the control flow for loops too but like is there a motive?)
-- |
-- | Although could we do this in the context of a function? given return statements
      -- > We could fake this, like so:

--       let statements = []

--       function() { writeStatements statements } -- then we can have a return statement

-- BEFORE:
-- if (cond1) {
--   return 1 
-- }

-- AFTER:
-- if (cond1) {
--    (function() { return 1 }) () 
-- }


-- Syntactically valid still and returns same result

-- Although we'd need to make sure our flow assigns this to the right variable

-- I suppose we could say that if cond1 == True then we will forsure run all these statements ((upto return))

-- runControl :: Control -> Maybe (Returned a)

    -- doWhile = do
    --   string "do"
    --   statements <- between' (char '{') (char '}') $ allowedStatementControl inFnScope 
    --   optional $ char '\n'
    --   string "while"
    --   many (char ' ')
    --   headExpr <- between1 (char '(') (char ')') jsExpression
    --   pure $ While headExpr (getFromStatements statements) statements






-- whileLoopInFn =
--   returnStatement
--   <|> whileLoopInFn
--   <|> forLoopInFn
--   <|> function -- i think this would just be normal cuz we aren't specially looking for the return statement for
--                -- the current scope
--   <|> ifStatementInLoop
--   <|> caseStatementInLoop 



-- I could just simply set up all top levels like this

-- whileLoop :: Bool -> ParsecT WhileLoop

-- where the Bool is whether or not we are *in* a function and thus can validly parse a return statement

-- And this seems to be the solution we must take as we cannot remove the return from its actual position
-- and such is valid. Even if extracted, it would lose meaning in the greater scope of the function

-- This also means that for our actual top level parser (when we are in the global context and not the fn context)
-- that we can just do:

--   whileLoop False
--   <|> forLoop False
--   ...

-- And while the returnStatement could be of the type :: ParsecT s u m TopLevel

-- the topLevel parser could simply not ask for it, so thus be able to parse 5/6 (or however many) Cases of TopLevel

-- And further, lets say we have

-- var x = (function(a){ ... ; return g })(arg)

-- then as long as we write this function out, we will be able to find the new value of x by the stdout trick
-- we've been using and in the case of no return statement (which may be a **conditionally desired result)
-- then this means x should be undefined

-- | Although function and class definitions that depend on state are in a sense
-- | unchanging, and will use the 'asked' state at the time of their application
-- isPure :: Expr a -> Bool
-- isPure = \case
--   Val _ -> True
--   Op _ _ _ -> False
--   _ -> False 

-- | TODO(galen): should this be a smart constructor?
-- | This is for testing purposes
-- | If true, can live in state 
isUnaffectedByState :: (Fractional a, Read a) => Expr a -> Bool
isUnaffectedByState = \case
  Val v -> isValuePure v
  -- PureOp _ _ _ -> True
  FuncAsExpr _ -> True
  ClassAsExpr _ -> True
  Op _ e1 e2 -> isUnaffectedByState e1 && isUnaffectedByState e2 
  Reference _ -> False
  ApplyFunc _ _ -> False 
  New _ _ -> False
  where isValuePure = \case
          Number _ -> True
          String' _ -> True
          Null _ -> True
          Boolean _ -> True
          Array (JSArray exprs) -> getAll . mconcat $ fmap (All . isUnaffectedByState) exprs
          Record (JSObject nvPairs) -> getAll . mconcat $ fmap (All . isUnaffectedByState . snd) nvPairs
          JSUndefined -> True 

-- | TODO: new {a : class {} }.a(1)
-- | TODO(galen): {a:1}["a"] AND [1,2,3][2] 
-- | TODO(galen): Stupid case where we have an expression that looks like a raw op
              -- let myArray = [x=1+1] ;console.log(myArray) --> [2]
-- as this recurses, we will need to add dependencies
-- | Actually this is only named expressions: for and while cannot be in this named context
jsExpression :: (Fractional a, Read a, Stream s m Char) => {-Maybe Name ->-} ParsecT s u m ((Expr a), [Dependency])
jsExpression = do
  (expr,deps) <- composable --whnfValue -- AppliedFunction | JSValue | existingConstructRef
  (optionMaybe $ (,) <$> (inSpace someOperator) <*> jsExpression) >>= \case
    Just (combinator, (expr2, deps2)) -> pure $ (Op combinator expr expr2, deps <> deps2)
    Nothing -> pure (expr,deps)
  where
    -- only optional for last line of script -> TODO: better
    --endsExpr = optional $ char ';' <|> char 
    
    composable =
      (try lambdaFunctionApp) -- has deps
      <|> (try existingFunctionApp)
      <|> (try functionAsOp)
      <|> (try classAsOp) 
      <|> (try objectInstantiation) -- has deps 
      -- | TODO(galen): we could actually pass names here to see if this has been decl.
      <|> (try $ ((,[]) . Reference) <$> ref)  -- name, or property, or fn as property with args 
      <|> ((,[]) <$> (Val <$> jsValue))

    -- | <class | function>AsOp is a weird case for our AST
    -- | because there is technically the ability to retrieve the inner name
    -- | But it's not accessible except by the var name in jsOperation
      -- | One possible way to validly represent this (ignoring future handling of evaluation)
      -- | is to duplicate names for normal functions and classes
        -- | eg. fromList [("NAME", Function "NAME" args stmts)] ala class too
    classAsOp = (((,[]) . ClassAsExpr) <$> jsClass)
    functionAsOp = (((,[]) . FuncAsExpr) <$> jsFunction)

    existingFunctionApp = do
      n <- jsValidName
      input <- jsArgTupleInput
      pure $ (ApplyFunc (Left n) (fmap fst input), mconcat $ fmap snd input)
    
    lambdaFunctionApp = do 
      FuncAsExpr f <- fmap fst $ between1 (char '(') (char ')') functionAsOp
      input <- between1 (char '(') (char ')') jsArgTupleInput
      pure $ (ApplyFunc (Right f) (fmap fst input), mconcat $ fmap snd input)
    
    objectInstantiation = do
      nameRefd <- sspace *> string "new" *> (some $ char ' ') *> jsValidName
      argsAndDeps <- option [] $ jsArgTupleInput
      pure $ (New nameRefd (fmap fst argsAndDeps), nameRefd : (mconcat $ fmap snd argsAndDeps))


-- | Note that a ref can refer to anything in the JSAST and is the main interface to the AST
-- | NOTE: deRef-ing means to take an Expr to a PureExpr and this is fundamental to all that we are doing 
ref :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Ref a)
ref = do -- Ref <$> jsValidRef <*> (inSpace $ optionMaybe jsArgTupleInput) <*> (inSpace $ optionMaybe (char '.') *> ref) 
  r <- jsValidRef -- name, will consume up to tuple 
  optionMaybe jsArgTupleInput >>= \case
    Just exprAndDeps -> do 
      let (exprs, depss) = unzip exprAndDeps
      -- only parse if we find the argtuple 
      mRef <- optionMaybe $ inSpace (char '.') *> ref
      pure $ Ref r (mconcat depss) (Just exprs) mRef
    Nothing ->
      --  In this case, we should never have a chain
      pure $ Ref r [] Nothing Nothing


--data DotRef a = Property Name | Fn (Function a) deriving (Show, Eq, Ord)
--data Ref' a = Prop Name | FnCall Name [(Expr a, [Dependency])] deriving (Show, Eq, Ord)
-- | New Idea:
--data FnCall' a = FnCall' Name  [(Expr a, [Dependency])] deriving (Show, Eq, Ord)
--data ObjectRef'' a = Ref'' [Name] (Maybe (FnCall' a)) deriving (Show, Eq, Ord)
-- | New Idea:
--data JSRef = TopLevelRef | ObjectRef deriving (Show, Eq, Ord)
-- | Although maybe FnCall should be it's own thing 
--type RefChain a = [Ref' a]  -- f(1).x(2).y AND `name` 


      -- parseCallsWith :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Name, [(Expr a, [Dependency])])
      -- parseCallsWith = do
      --   refdName <- jsValidName
      --   args <- option [] jsArgTupleInput
      --   pure (refdName, args) 

    



someOperator :: Stream s m Char => ParsecT s u m Operator
someOperator =
  (Multiply <$ (string "*"))
  <|> (Plus <$ (string "+"))
  <|> (Subtract <$ (string "-"))
  <|> (Exponentiation <$ (string "**"))
  <|> (Division <$ (string "/"))
  <|> (Modulo <$ (string "%"))
  <|> (Equal <$ (string "=="))
  <|> (EqualSameType <$ (string "==="))
  <|> (NotEqual <$ (string "!="))
  <|> (NotEqualOrNotSameType <$ (string "!=="))
  <|> (GreaterThan <$ (string ">"))
  <|> (LessThan <$ (string "<"))
  <|> (LessOrEqual <$ (string "<="))
  <|> (GreaterOrEqual <$ (string ">="))
  <|> (Ternary <$ (string "?"))
  <|> (AND <$ (string "&&"))
  <|> (OR <$ (string "||"))
  <|> (NOT <$ (string "!")) -- Should i make the second Expr a Maybe Expr? I may have to
  <|> (ANDB <$ (string "&"))
  <|> (ORB <$ (string "|"))
  <|> (NOTB <$ (string "~"))
  <|> (XORB <$ (string "^"))
  <|> (Shift <$> shift)
  where
    shift = (try $ string ">>") <|> (try $ string "<<") <|> (try $ string ">>>")


assignmentOp :: Stream s m Char => ParsecT s u m AssignOp
assignmentOp = do
  (A_Equal <$ string "=")
  <|> (A_Plus <$ string "+=")
  <|> (A_Subtract <$ string "-=")
  <|> (A_Multiply <$ string "*=")
  <|> (A_Divide <$ string "/=")
  <|> (A_Modulo <$ string "%=")
  <|> (A_Exponentiation <$ string "**=")
  <|> (A_LShift <$ string "<<=")
  <|> (A_RShift <$ string ">>=")
  <|> (A_UnsignedRShift <$ string ">>>=")
  <|> (A_ANDB <$ string "&=")
  <|> (A_XORB <$ string "^=")
  <|> (A_ORB <$ string "|=")
  <|> (A_AND <$ string "&&=")
  <|> (A_OR <$ string "||=")
  <|> (A_Nullish <$ string "??=")
      
  
      


-- jsOperation = do
--   jsExpression
--   many (char ' ') 
--   (oneOf ['\n', ';']) <|> do 
--     combinator <- someOperator
--     someValueWHNF <- jsOperation
    


--data [] a = [] | a : []
--data Op = Op Operator Expr' Expr'
-- TODO(galen): combine with Expr so that this can be a recursively defined type
  -- OpConstructor Operator Op Op 



-- | Is an Operation as expressed to an expression because of 
jsIterator :: Stream s m Char => ParsecT s u m (JSOperation a)
jsIterator = undefined
  -- should include both
  -- i++
  -- i+=<VAL>

data VarType = Raw' -- literally is none (eg: with x as defined obj. : x.r = 1)
               -- Same behavior as Let
             | Let'
             | Var'
             | Const'
             



-- | This is an Expression which may be named
-- | This can and should be viewed as the main interface to creating the AST (and updating in the case of
-- | iterators)
-- | TODO: let z; 
-- | TODO: (let z = class {} --> creates: class z)
-- | TODO(galen): iterators (eg: x += 1 ) 
-- | TODO(galen): obj.newField = <VAL>
  ---data JSOperation a = JSOperation [Dependency] (Maybe [Name]) (Expr a) --JS
--data JSOperationNEW a = JSOperationNEW [Dependency] (Maybe (VarType, [Name])) (Expr a)
--data JSOperation a = JSOperation [Dependency] (Maybe Name) (Expr a) --JS

-- jsOperation :: (Fractional a, Read a, Stream s m Char) => dependencies  -> ParsecT s u m (JSOperation a)
-- jsOperation dependencies = do
--   mName <- optionMaybe jsVarName -- todo: assignment operators -- TODO multiple vars per var keyword
--   (expr, deps) <- jsExpression
--   -- | Either followed by ; or \n
--   -- | oneOf ['\n', ';']
--   -- | and only here; this doesnt apply to top levels with {} (although it may apply to return and break)
--   sspace >> (char '\n' <|> char ';')
--   pure $ JSOperation deps mName expr


-- | For context: at toplevel: this is tried last
jsVarName :: Stream s m Char => ParsecT s u m Name 
jsVarName = do
  (try $ string "let ") <|> (try $ string "var ") <|> (try $ string "const ")
  many (char ' ')
  name <- jsValidName 
  manyTill_ (char ' ') (char '=')
  many (char ' ')
  pure name

jsValidName :: Stream s m Char => ParsecT s u m Name 
jsValidName = do
  first <- letter <|> (char '_') <|> (char '-') 
  rest <- many $ alphaNum <|> (char '_') <|> (char '-')
  pure $ first : rest

jsValidRef :: Stream s m Char => ParsecT s u m [Name]
jsValidRef = sepBySome jsValidName (char '.') 

sepBySome p s = f =<< sepBy p s
  where
    f = \case
      [] -> parserFail "no matches"
      (x:xs) -> pure (x:xs)

-- | This cannot assign Raw as this doesnt parse enough context for that
-- | that's told by jsOperation
varType :: Stream s m Char => ParsecT s u m (Name -> VarDecl)
varType = 
  (Let <$ (try $ string "let")) <|> (Var <$ (try $ string "var")) <|> (Const <$ (try $ string "const"))

--data JSOperation a = JSOperation [Dependency] (Maybe Name) (Expr a) --JS
jsOperation :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m [JSOperation a]
jsOperation  = do
  vType <- optionMaybe $ inSpace varType
  case vType of

    --vType and special operators are mutually exclusive 
    
    Just vType' -> 
      flip sepBySome (char ',') $ do
        n <- jsValidName -- NOT jsValidRef; illegal with vartype 
        (expr,deps) <- option (Val $ JSUndefined,[]) $ (char '=') *> jsExpression
        pure $ JSOperation deps (Just (vType' n)) expr
    Nothing ->
      -- | TODO: Only in this case could we find iterators
      -- Only in this case could we find x.y.z..
         -- Note; that these dont need to exist as long as the parent does 
      assignOp <|> nakedExpr
      where
        assignOp = do
          flip sepBySome (char ',') $ do
            (n,op) <- (,) <$> jsValidRef <*> assignmentOp -- disallows plain expressions
            {- FOR OTHER non '=' operators:
                 let o = show operator in expr= <var> o <expr> 
            -}
            (expr, deps) <- jsExpression
            pure $ JSOperation deps (Just (Raw op n)) expr
        
        nakedExpr = do
          (expr,deps) <- jsExpression
          pure $ [JSOperation deps Nothing expr]
      -- WAIT A MINUTE! Why dont we view this as MAYBE prefixed by (name <* char '=')

    -- NOTE: the only thing that is actually illegal is to do this:
  -- x ; (where x is not defined)
  -- LEGAL:
  -- <let|var> x
    -- However: const x = <expr> -- expr is required here
  -- x = <expr>
  -- <vartype> x = <expr>
  -- let x,y;  


  -- IF no expr -> JSUndefined
  -- IF no 

  -- maybeVarType
  -- someSep (do
  --            name
  --            option JSUndefined (char '=' >> jsExpression)
  --
  -- Except that this could still be a false positive for a plain expression

  -- IF doesNotExists(varType) then we must see an equal sign

  -- FURTHER: the real answer is to check all possibilities of a named expression
  -- and then accept a non-named expression
  -- based on scriptTest: "let x={a:1}; x.a" 



  
  -- vType <- option Raw varType
  -- nedList <- someSepBy $ do
  --   name <- varNameWhereDotsAreAllowed  -- can return 1 or more matches
  --   char '=' >> jsExpression -- NECESSITY
  --   (expr,deps) <- 
  --   pure (name,expr,deps)

  -- pure $ fmap (\(n,e,d) -> JSOperation ) nedList 
      
  -- mName <- optionMaybe jsVarName -- todo: assignment operators -- TODO multiple vars per var keyword
  -- (expr, deps) <- jsExpression
  -- -- | Either followed by ; or \n
  -- -- | oneOf ['\n', ';']
  -- -- | and only here; this doesnt apply to top levels with {} (although it may apply to return and break)
  -- sspace >> (char '\n' <|> char ';')
  -- pure $ JSOperation deps mName expr

  
      
  

-- | NOTE!!! This and all loops can have a return statement if they are inside a function
--whileLoop = undefined

--forLoop = undefined
objectDeclaration = undefined
  -- basically just properties where you can have functions

-- NOTE: Need to make a concept like jsStatement except it 


-- data While a = While Condition [Dependency] [JSOperation a]
-- whileLoop :: Stream s m Char => ParsecT s u m (While a) 
-- whileLoop = do
--   (try normal) <|> doWhile
--   where

--     normal = do
--       string "while"
--       headExpr <- between1 (char '(') (char ')') jsExpression
--       statements :: _ <- between' (char '{') (char '}') jsStatement
--       -- pure $ While headExpr statements statements
--       pure ()
      


      
-- | Note that is purely done out of weird timing
-- | as long as I have the Expression as 
-- jsExpression :: ParsecT s u m [ExprPiece]
-- jsExpression = do
--   manyTill piece (char '\n' <|> char ';')
--   where
--     piece = jsVal -- can scrape for deps
--             <|> domRef 
--             <|> operator  -- should include typeof
-- 	    <|> objectInstantiation -- obj dep 
-- 	    <|> anonFunc  
-- 	    <|> indexedValue -- update
-- 	    <|> functionApp -- fun dep
-- 	    <|> varRef -- dep
-- 	    <|> deletion -- update
-- 	    <|> (bracketTree (char '(') (char ')'))
-- 	    <|> wordOperators -- instanceof, in, void, typeof 
-- 	    <|> thisOrSuperReference
-- 	    <|> objectRef
-- 	    <|> returnStatement
--             <|> importStatement
--
--     operator = logical
--                <|> bitwise
-- 	       <|> iterational -- like ++
-- 	       <|> math
--
--      -- this parser must come after we've done domRef
--     varRef = alpaNum >> maybeParse (char '.' >> varRef)
--
--     jsValue = aeson with .property possibility 
          

      
        -- where
        --   withDefaultArg = do
        --     name <- jsValidName
        --     manyTill_ (char ' ') (char '=')
        --     many (char ' ')
        --     v <- jsValue
        --     pure $ ArgDef name v

        --   jsArgName = ArgName <$> jsValidName

-- objects and functions in terms of a chain are more like interchangeable pieces where objects are just scopes

-- like i can do

-- f(1) 

-- unless const myobject = { f : function(x){ return x } }

-- then I must do:

-- myobject.f(1) 
  
-- but it can also be

-- myobject.g

-- and also objects are interesting because the question, what code do we need to run?
--   1) Constructor
--   2) g, or rather the .<jsName>'s
--      and whatever constructs are referenced by the scopes
--      2.1) I suppose you could say that for the expr: a.b.c.d:
--           -> if d references a construct in c or b or a then we need it
--           -> Although shitty method: can just run all of a 
  

-- var x = y -> Ref "y" []
-- var x = y.z -> Ref "y" [Property "z"]
-- var x = y.z.z2.a(1) -> Ref "y" [Property "z", Property "z2", Fn (Function [Arg a] [someDeps] [someStatements])


-- --data Function a = Function [Arg a] [Dependency] [Statement]
-- (Maybe [Arg])


-- Name <+> f <+> f .. 

-- where f = (Nothing <|> Name <|> Function)


-- TopLevelRef <+> Recursion



-- (char '.') >> (Name <|> Function) 

-- (this also definitively seperates it from being a plain top-level function call) 

-- f = do
--   name <|> function
--   mF <- optionMaybe f
--   case mF of
--     Nothing ->  


-- Should this also be a function?
-- intercalate "." names 
--data Ref = Ref Name [Arg a] [DotRef a]

    
    
    
    -- functionApplication :: Stream s m Char => ParsecT s u m FunctionRef
    -- functionApplication = parseCallsWith 

    -- existingConstructRef :: Stream s m Char => ParsecT s u m ObjRef
    -- existingConstructRef = do
    --   n <- jsValidName
    --   option [] $ jsTupleArgs
    --   optionMaybe f
    --   where
    --     f = do
    --       char '.'
    --       n <- jsValidName
    --       option [] $ jsTupleArgs 
    --       maybeMore <- optionMaybe f
    --       case maybeMore of
    --         (x:xs) :: DotRef -> Add to overall dot ref
    --         Nothing -> we've only scoped in once i guess so return x 
          


      
    --   --either (name <|> nameWithArgs <|> nothing)
    --   cont <- optionMaybe (name <|> nameWithArgs)
    --   case cont of
    --     Nothing -> -- is simple property
    --       pure $ ObjRef [n] []
    --     Just (ObjRef names dotRef) ->
    --       pure $ ObjRef (n:names) dotRef

    --   where
    --     nothing = mzero 
    --     name = do
    --       jsValidName
    --       option [] $ (char '.' >> existingConstructRef)

    --     nameWithArgs = do 
    --       jsValidName

    --       jsTupleArgs -- not really
    --       option [] $ (char '.' >> existingConstructRef)
    --       -- or
    --       -- jsCallFunction
 

    -- existingObjRef :: Stream s m Char => ParsecT s u m ObjRef
    -- existingObjRef = do
    --   jsValidName -- if it's a name, we know that it must be a reference
    --   many (char '.' >> jsValidName)
    --   between1 (char '(') (char ')') $ jsExpression 

-- foreign import javascript unsafe
-- "1+1" 

-- foreign import javascript unsafe "$1 + $2" add :: Int -> Int -> Int



---------HIGHLIGHTS - Maybe Compilable ?--------------------------------------------------------------------------
-- | Execution model could be that I filter out things that can be lazy and only run
-- | statements that would potentially cause an effect

-- | filtered       :    Function, Object, 
-- | executed direct:    WhileLoop, ForLoop, Exec, CaseStatement



-- | I could also use bracketTree in JSExpression

-- | let c = { 'a' : 5 }  --> VarDecl Object-- | let c = 1 --> VarDecl Int




-- parseStatement :: Text -> Statement
-- parseStatement = undefined

-- --Map Name JSValue 
-- execJS :: MonadJS m => [JSStatement'] -> m () 
-- execJS = do undefined
  
  -- |case js of
     -- Runnable -> do runJSWithCli =<< (substitute js) <$> get 
     -- NotRunnable -> put to state
     -- Iterator -> update state where state :: Map Name JSValue
     -- LocationChange -> undefined
     --   -- this JS Context is now Void except Window object 


---------HIGHLIGHTS - Mayve Compilable ?--------------------------------------------------------------------------

-- data Statement = Statement [Dependency] (Maybe Name) JS
-- data JSStatement' =
--   WhileLoop' [Dependency]
--   | ForLoop' [Dependency] RawJS -- note that multiple 'lets' could be used
--   --		 | CaseStatement (Map Condition RawJS) -- would also need ?: syntax
--   | Function'' Name RawJS
--   | ObjectDeclaration Name RawJS
--   | Exec JSExpression -- in current top level as raw statement, no assignment
--   | VarAssign Name JSExpression -- this could be an empty expression
--   | Iterator  

-- -- this is derived from stream editing 
-- data JSExpression = JSExpr [Dependency] RawJS 


-- data JSValue' = JSValue' JSStatement' [Update]

-- data Update = Reassign JSExpression
--             | Iterate JSStatement'


-- data ExprPiece = ExprPiece String 
-- -- jsSetValue :: ParsecT s u m ((Name, [Dependency], [ExprPiece])) 
-- jsSetValue = do
--    manyLeftSide
--    maybeParse (assignmentOperator >> expression) 

--    where assignmentOperator = char '='
--                             <|> (otherOperator >> (char '='))

--data JSString = JSString String deriving (Show, Eq, Ord)




--jsFunction = undefined
--jsClass = undefined
jsSubClass = undefined
jsWhileLoop = undefined
jsForLoop = undefined
jsIfStatement = undefined
jsSwitchStatement = undefined 
jsObjectInstantiation = undefined
jsObjectDefinition = undefined


-- | Note that is purely done out of weird timing
-- | as long as I have the Expression as 
-- jsExpression :: ParsecT s u m [ExprPiece]
-- jsExpression = do
--   manyTill piece (char '\n' <|> char ';')
--   where
--     piece = jsVal -- can scrape for deps
--             <|> domRef 
--             <|> operator  -- should include typeof
-- 	    <|> objectInstantiation -- obj dep 
-- 	    <|> anonFunc  
-- 	    <|> indexedValue -- update
-- 	    <|> functionApp -- fun dep
-- 	    <|> varRef -- dep
-- 	    <|> deletion -- update
-- 	    <|> (bracketTree (char '(') (char ')'))
-- 	    <|> wordOperators -- instanceof, in, void, typeof 
-- 	    <|> thisOrSuperReference
-- 	    <|> objectRef
-- 	    <|> returnStatement
--             <|> importStatement
--
--     operator = logical
--                <|> bitwise
-- 	       <|> iterational -- like ++
-- 	       <|> math
--
--      -- this parser must come after we've done domRef
--     varRef = alpaNum >> maybeParse (char '.' >> varRef)
--
--     jsValue = aeson with .property possibility 

-- |
-- value :: ParsecT s u m JSString 
-- value = do
--   tuple
--   <|> jsBracketTree
--   <|> string
--   <|> array
--   <|> bool
--   <|> null 
--   <|> number
--   where
--     number = undefined
--     bool = undefined
--     array = undefined jsArray
--     jsBracketTree = bracketTree
--     tuple = bracketTree undefined
--     null = undefined
--     string = undefined
  -- <|> reference

-- TODO(galen): try solving annoying Show problem by using Char vs Parser Char as args
-- jsArray :: Stream s m Char => ParsecT s u m String
-- jsArray = bracketTree (char '[') (char ']') 

-- | We should check that this isnt a function or class before we parse it 
-- where Dependencies would come from
 
-- -- This allows me to specially get args from tuples 
-- tuple :: JSTuple 
-- tuple = do
--   o <- (:[]) <$> open
--   (args, end') <- manyTill_ ((tuple <|> many (noneOf [',', ')'])) <* (optional $ char ',')) end
--   pure $ JSTuple args (o <> (mconcat $ intercalate "," args) <> end')
--    --(noneOf [',', ')'])
--   where
--     open = (:[]) <$> char '('
--     end = (:[]) <$> char ')'

jsInnerParser :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
jsInnerParser open close = do
  (src, bracket) <- manyTill_ (try (bracketTree open close) <|> ((:[]) <$> anyChar)) close
  pure (mconcat src <> (bracket :[]))

bracketTree :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
bracketTree open close = do
  open
  inner <- jsInnerParser open close
  pure inner



-- WhileLoop + DoWhileLoop
-- ForLoop
-- IfStatement
-- SwitchStatement
-- Function
-- ObjectDeclaration


-- Iterator
-- VarDeclaration
-- JSExpression -- including Object instantiation
-- DomRef -- ~JSExpression

-- type RawJS = String 



-- And this is an atomic step we can take 

-- tuple = undefined


-- with destructuring
-- var [one, two, three] = foo;

-- Also possible: constrained by must be left hand side 
-- let g = x['hey'] = f();

   -- so this can happen as long as there is no right hand side operations 

-- there's also an indexed assignment

-- let x[1] = 4 -- we can consider this an update to x



-- one other possible way I could handle Objects is that if i keep the object
-- then I can see how the object is used and if lets say the following happens:

-- let x = new Object()
-- let y = x.someMethod(1)

-- We scrape the inner function of .someMethod then apply it like a normal function
-- but we'd need to handle for some cases where the object is constructed


-- -- note: we will need a SuperClass parser (a Extends b)

-- let x = 1

-- let ob = new SomeObject(x)

-- ob.func(23)

-- let y = ob.result



-- class SomeObject {
--     constructor {}

--     a = 

-- }



-- let x = new Obj() -- this calls constructor

-- let g = x.r(3)
-- let h = x.r(3)

-- g === h ? 

-- x.addIn(3)
-- -- this is an expression that would force all updates on x from the time of instantiation
--   -- so it may or may not affect some demanded 





-- for DOM updates, how I can handle related memory of the DOM that would depend on Update OR Deletion
-- like if a parent is deleted , the child should not be found

 -- Remedy:
--   streamEdit the DOM with all updates to TimePoint t
--    -> when a DOM element is asked for


-- if we actually update the State of JSMonad then we have sharing

-- let y = x + 1 -- this would force computation of y and x, if y is ever evaluated in a dependency


-- x += 1

-- let z = x + 1


-- If I really was motivated to make this happen I could track when the statement exists
-- including updates (would also have a tracked step Num)

-- It wouldnt matter that the Dependency itself had an update after the time that this Dependency was used


-- let x = 1
-- let y = x
-- x += 1


-- y == 1

-- so: if we have X stored as

--   "x" : (origin, [update1, update2, update3])

--   and it is forced to evaluate:

--   in the process we find x == 6

--   so:
--     "x" : (6, [])

--   and continues like a newly instantiated variable would



-- let x = 1
-- let y = x -- x == 1 (for all eternity) 
-- x += 1  -- x == 2 
-- let z = x

-- x += y 


-- So:

--   "x" : ((1, initialValueX), [(3, "x+=1"), ...])

--    AND

--    "y" : ((2, initialValueY), [])

--    a var will take from its dependencies up until when it was instantiated -- But shit this doesnt work for Objects
--    (2 < 3) so it doesnt run x+=1 to get its Deps value 


--    We can optimize for sharing by replacing the *how* to get to State_i with the value of it

--    so X would be replaced with

--    "x" : ((1, NF 1), [(3, NF 2), (16, WHNF "x+=y")]



-- Map Ref JSValue

-- data Ref = VarRef Name
--          | FuncRef Name
--          | ClassRef Name [Dependencies] deriving Ord -- this is the Key for the JSAST






-- -- Need to redo func to process conditionals

-- -- Need to redo execFuncEnv
--   -> This will call some statements with all global variables and the given required vars



  
  


-- We could function-ize loops by scraping the affected variables in the loop statement and just console.logging the
-- value of these referenced variables 

-- switch ~~ If where (==Case)
-- and case is oneof JSValues

-- So in implementation switch will just parse to a conditional with (==Case) set


-- SetVar is significant in implementation because it affects global state

-- all SetVar is tho is (Name, JSExpression)

-- SetVar has a specific form but can be any JS and so we'd need to handle the following too:

--    let f = function fAnonymousUnreffable(x,y,z) { ... }  ~~~=~~~ function f(x,y,z) 

--    -- same with class


-- An easy way I could handle Variables when running them (before laziness is implemented) is just run in the node
-- cli then grab the var being set via console.log

-- WE could differentiate between instantiation logic and update logic

-- where update logic is lazy and compressable / WHNF and instantiaton logic is as follows:

-- if object
-- then do nothing yet
-- else jsExpression -> eval jsExpression so that var x = 1 or similar


--    JSInstance -> (x, Value 1) | (x, NewObject ClassName) 

--    JSValue = JSI JSInstance [Update]

    


-- when

-- obj.something(...)

-- this can both return a value and update internal state

-- so the following is valid JS

-- let x = someObj.result()

-- SO this statement should qualify as an update and a SetVar where x == all updates to now

-- and the update is stored as:

-- someObj.result() -- so result is thrown out but update would persist in this evaluation


-- and ofc type Update = JSStatment


-- -- Functions

-- Will be of the shape:

-- Function Name [ArgNames] RawJS

-- 1) Inject LocalVars with ArgNames --labelled to not be console.logged
-- 2) Go until I run out of statements || case(ReturnStatement) 

-- -- A function has the ability to affect global state
-- -- we determine this by the variables updated and declared (varname = value) 
-- runFunction :: JSAST -> JSASTLocal -> MonadJS (Maybe a)

-- runFunction a b = runMonadJS (a <> b)


-- A shitty way I could implement return functionality is when in a function use the normal eval func I make
-- but the eval func:

-- scrappyEvalJS = do
--   ...
--   case statement of
--     ...
--     Return _ -> error "invalid return"


-- and so runFunction:

--   case statement of
--     Return a -> pure . Just $ a
--     s -> scrappyEvalJS s



-- For objects even tho the thing itself is complicated we only need to know dependencies when evaling
-- a statement

-- So if we can translate a dependency on a class to a function statement (which has been preceded with the constructor
-- being evaluated into a local JSASTLocal)

-- class Thing { 
--  constructor() { A }
--  function() { B }
--  }

-- effectively becomes:

--   statements = A `then` B :: [JSStatement] -> MonadJS (Maybe a)

--   and this.* will be the names of the constructor memory

--   and I need to do this in order to run statement by statement 


-- WE also need to have a special way to parse function apps and object insts

-- new Obj(dep1, dep2, dep3..)

-- f(dep1, dep2, dep3)


-- The difference between an object and a normal variable is that the initial value is always a real
-- expression (1 may be an expression that simply evals to 1) whereas for an object it is the result of
-- an AST that has been affected by the constructor

-- type Object = JSAST

-- So while a function also has a local JSAST, an Object persists this across time

-- so I could store as:


--   let x = new Obj()

--   "x" : (resultConstruct :: Object JSAST, [] :: [Update])  -- in a Map Ref ((Object | JSValue),  [Update])

-- data SomeValue = JSValue RawJS
--                | Object ([Args] -> Map Name SomeValue) RawJS  

-- vdata JSAST = JSAST { varRefs :: Map Name (SomeValue, [Update]) -- includes object instances 
--                    , functionRefs :: Map Name RawJS
--                    , objectRefs :: Map Name Constructor RawJS
--                    }


-- type Update = SomeValue -> SomeValue 

-- State = Object JSAST | JSValue RawJS




-- -- calling a JS function for an expression

-- a <*> b <*> (fromMaybe "undefined" <$> runMonadJS name (localAst <> globalAst)) <*> ... <*>  
