{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-} 

{-|
Known Exceptions:

Does not handle Object.*(someObj, js) cases (of Type Update)

|-}

module JS.JSFFIParse where

import JS.Types (Name)
import JS.Source
import JS.MonadJS

import JS.JSFFIParse.Value
import JS.JSFFIParse.Combinators

import Control.Applicative (some, liftA2, empty )
import Text.Parsec
import Data.Map (Map)
import Data.Text (Text)
import Data.List (intercalate)

testVarString = "var ue_id = 'WEBRWTDQ8PSAD8KS77XA'"

-- test1 = parse (jsVarName >> jsString ) "" testVarString
-- test3 = parse (jsVarName >> jsBool ) "" "var ue_navtiming = true"
-- test4 = parse (jsVarName >> jsNull ) "" "var ue_navtiming = null"

jsArgTuple :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m [ArgName a]
jsArgTuple = do
  char '(' *> sepBy (jsArgName <|> withDefaultArg) (char ',') <* char ')'
      
      
  where
    withDefaultArg = do
      name <- jsValidName
      manyTill_ (char ' ') (char '=')
      many (char ' ')
      v <- jsValue
      pure $ ArgDef name v

    jsArgName = ArgName <$> jsValidName
      

-- data JSStatement' = WhileLoop [Dependency] RawJS
--      		  | ForLoop [Dependency] RawJS -- note that multiple 'lets' could be used
-- 		  | CaseStatement (Map Condition RawJS) -- would also need ?: syntax 
-- 		  | Function' Name RawJS
-- 		  | ObjectDeclaration Name RawJS
-- 		  | Exec JSExpression -- in current top level as raw statement, no assignment
-- 		  | VarAssign Name JSExpression -- this could be an empty expression 
-- 		  | Iterator 



-- | All of these pieces are mutually recursive in that they may contain each other
-- | For Function and Class: definitions dont escape scope
-- data JSTopLevel a = Function' 
--                   -- Contains: all 
--                   | While (WhileLoop a)
--                   | For
--                   | Class
--                   | Operation -- rename Exec
--                   | Switch
--                   | IF
--                   | TryExcept
--                   | Return (Expr a)
--                   -- Can actually be a Expr, Class or Function
--                   -- but cannot be IF, Switch, TryExcept, or Loop


            
-- data JSTopLevel = Object' Object@( Function' | Class | Operation(cuz var) )
--                   | Control ( While | For | Switch | TryExcept )
--                   | Return Object 
                
--data Statement = Statement [Dependency] (Maybe Name) JS


--jsOperation = jsStatement

jsTopLevelStatement :: Stream s m Char => ParsecT s u m (JSTopLevel a)
jsTopLevelStatement = undefined
-- -- | Any top level piece that must be run altogether for actual use and desired behavior
-- -- | All of these 5 cases may recurse into each other 
-- jsTopLevelStatement :: Stream s m Char => ParsecT s u m a
-- jsTopLevelStatement =
--   -- By design all statements which start with keywords are run first since this gives the greatest integrity
--   -- the inspo for this is that anon functions may be named
--   -- eg: var x = function (<name> | "")(x,y,z) {..}
--   -- and this <name> is not available at top level 
--   jsFunction
--   <|> whileLoop
--   <|> forLoop
--   <|> objectDeclaration -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes
--   <|> (jsOperation undefined)



type InFnScope = Bool




--What if instead we had

-- | For control ones, we don't need or even want to know dependencies at this level
-- | For Functions and Classes we do special scoping where when we check dependencies
-- | We first check if the function defined the var name
  -- | And prefer the innermost scope (for multiple functions)
  -- | (function (x) { var x = 2; return ((function(x){return x})(10))})(1) ---> Returns 10 NOT(2,1)
  -- | `this` is a synonym for the outer object (?)
  
data JSTopLevel a = Control' (Control a)
                  | Declare (Object a)
                  | Return (Object a) -- Weird case
              
data Control a = While (WhileLoop a)
               | For (ForLoop a){-todo:(ForLoop a) -}
               | IF (IFStatement a)
               | Switch
               | TryExcept 
               -- | Return 

data Object a = Function' (Function a)
              | Class {-todo:make-}
              | Operation (JSOperation a)

returnStatement :: Stream s m Char => ParsecT s u m (JSTopLevel a)
returnStatement = undefined

allowedStatementControl :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (JSTopLevel a)
allowedStatementControl inFnScope =  
  mReturnStatement 
  <|> (Declare <$> (jsFunction <|> jsClass <|> (Operation <$> (jsOperation undefined))))
  <|> (Control' <$> (control inFnScope))
  where 
    mReturnStatement = if inFnScope
                       then (returnStatement)
                       else empty

    control :: (Fractional a, Read a, Stream s m Char) => Bool -> ParsecT s u m (Control a)
    control inFnScope =
      (While <$> whileLoop inFnScope)
      <|> (For <$> forLoop inFnScope)
      <|> switchStatement inFnScope
      <|> tryExcept inFnScope
      <|> (IF <$> ifStatement inFnScope )

    jsFunction = undefined -- for now


data ForLoop a = ForLoop (Maybe (JSOperation a), (Condition a, [Dependency]), Maybe (JSOperation a)) [JSTopLevel a]
forLoop :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (ForLoop a)
forLoop inFnScope = do
  triplet <- forHead
  topLevels <- between' (char '{') (char '}') $ allowedStatementControl inFnScope
  -- NOTE: the topLevels may contain the iterator and the control var may be set before the loop
  pure $ ForLoop triplet topLevels
  where
    forHead = do
      inSpace $ char '('
      mControlVariable <- optionMaybe $ jsOperation undefined
      inSpace $ char ';'
      exitCondition <- jsExpression
      inSpace $ char ';'
      mIterator <- optionMaybe $ jsOperation undefined -- probably gonna opt not to pass anyways 
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
data IFStatement a = IFStatement [(Condition a, [JSTopLevel a])] -- in order 
ifStatement :: Stream s m Char => InFnScope -> ParsecT s u m (IFStatement a)
ifStatement inFnScope =  undefined
  -- -- definitely an if clause
  -- -- many else if clauses
  -- -- once no more else if's, maybe an else
  -- ifClause
  -- -- many elseIf
  -- -- maybeElse
  -- where
  --   expression = inSpace (char '(')  *> jsExpression <* inSpace (char ')')
    
  --   ifClause = undefined
    
  

  

tryExcept inFnScope = undefined
switchStatement inFnScope = undefined
--whileLoop inFnScope = undefined

data WhileLoop a = WhileLoop (Condition a, [Dependency]) [JSTopLevel a]
whileLoop :: (Fractional a, Read a, Stream s m Char) => InFnScope -> ParsecT s u m (WhileLoop a) 
whileLoop inFnScope = do
  (try normal) <|> doWhile
  where    
    normal = do
      string "while"
      headExpr <- between1 (char '(') (char ')') jsExpression
      statements <- between' (char '{') (char '}') $ allowedStatementControl inFnScope
      pure $ WhileLoop headExpr statements
    
    doWhile = do
      string "do"
      statements <- between' (char '{') (char '}') $ allowedStatementControl inFnScope
      optional $ char '\n'
      string "while"
      many (char ' ')
      headExpr <- between1 (char '(') (char ')') jsExpression
      pure $ WhileLoop headExpr statements

-- | Arg is just meant to represent names for special scopes, not args passed in a statement
data ArgName a = ArgName Name | ArgDef Name (JSValue a)
--data Function a = Function (Maybe Name) [ArgName a] [Dependency] [JSStatement] (Maybe (Return a))
data Function a = Function (Maybe Name) [ArgName a] [Dependency] [JSTopLevel a] 

-- | 
-- Function
-- Bracket <+> Function <+> Bracket <+> ArgTuple 
--data Function a = Function (Maybe Name) [ArgName a] [Dependency] [Statement]
-- ( __f__ <|> __fT2__ <|> fT3 )(tuple)
-- NOTE a function doesn't need all arguments to run and if it cannot compute the return value -> undefined
-- jsFunction :: (Fractional a, Stream s m Char) => ParsecT s u m (Function a)
-- jsFunction = do
--   -- In general, I dont think this should call jsTopLevel but rather some (maybe all) of the same parsers
--   -- that JS top level would and add a case via eitherP that allows matching on a return statement
--   -- that is treated as a special case only for this parser
--   -- NOTE: for now it's fine that most parsers will be undefined such as loops
--   --
--   -- NOTE: This could also define a function inside it, which has it's own returnStatement (nesting) 
--   anonArrow <|> normalFunc
--   where
--     returnStatement = undefined
--     fnLoops = undefined -- validloopStatementTopLevels <|> returnStatement
--     fnCaseStatement = undefined -- valdCaseStatementTopLevels <|> returnStatement 
    
-- --     anonArrow = do
-- --       undefined
      
    
--     normalFunc = do
--       (fnName, args) <- functionHead
--       (bodyStatements, deps) <- functionBody args
--       pure $ Function (Just fnName) args deps bodyStatements
--       where
--         functionHead = do
--           string "function"
--           mFnName <- optionMaybe jsValidName
--           args <- jsArgTuple
--           pure (mFnName, args) 

--         functionBody headArgs = do
--           between' (char '{') (char '}') $ (jsTopLevelStatement headArgs <|> returnStatement)
            


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



             
--data Function a = Function (Maybe Name) [ArgName a] [Dependency] [Statement]


jsBracketed :: ParsecT s u m ()
jsBracketed = undefined


    


  
      


-- jsOperation = do
--   jsExpression
--   many (char ' ') 
--   (oneOf ['\n', ';']) <|> do 
--     combinator <- someOperator
--     someValueWHNF <- jsOperation
    

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




parseStatement :: Text -> Statement
parseStatement = undefined

--Map Name JSValue 
execJS :: MonadJS m => [JSStatement'] -> m () 
execJS = do undefined
  
  -- |case js of
     -- Runnable -> do runJSWithCli =<< (substitute js) <$> get 
     -- NotRunnable -> put to state
     -- Iterator -> update state where state :: Map Name JSValue
     -- LocationChange -> undefined
     --   -- this JS Context is now Void except Window object 


---------HIGHLIGHTS - Mayve Compilable ?--------------------------------------------------------------------------

data Statement = Statement [Dependency] (Maybe Name) JS
data JSStatement' = WhileLoop' [Dependency] RawJS
     		 | ForLoop' [Dependency] RawJS -- note that multiple 'lets' could be used
--		 | CaseStatement (Map Condition RawJS) -- would also need ?: syntax 
		 | Function'' Name RawJS
		 | ObjectDeclaration Name RawJS
		 | Exec JSExpression -- in current top level as raw statement, no assignment
		 | VarAssign Name JSExpression -- this could be an empty expression 
		 | Iterator 





type Condition a = Expr a
--data Condition = Condition RawJS

-- this is derived from stream editing 
data JSExpression = JSExpr [Dependency] RawJS 


data JSValue' = JSValue' JSStatement' [Update]

data Update = Reassign JSExpression
     	    | Iterate JSStatement'


data ExprPiece = ExprPiece String 
-- jsSetValue :: ParsecT s u m ((Name, [Dependency], [ExprPiece])) 
-- jsSetValue = do
--    manyLeftSide
--    maybeParse (assignmentOperator >> expression) 

--    where assignmentOperator = char '='
--                             <|> (otherOperator >> (char '='))


data JSTuple = JSTuple [JSString] 




--jsFunction = undefined
jsClass = undefined
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
jsArray :: Stream s m Char => ParsecT s u m String
jsArray = bracketTree (char '[') (char ']') 

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
