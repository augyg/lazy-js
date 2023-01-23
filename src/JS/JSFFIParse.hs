{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-} 

{-|
Known Exceptions:

Does not handle Object.*(someObj, js) cases (of Type Update)

|-}

module JS.JSFFIParse where

import JS.Types (Name)
import JS.Source
import JS.MonadJS

import Control.Applicative (some, liftA2)
import Text.Parsec
import Data.Map (Map)
import Data.Text (Text)
import Data.List (intercalate)

-- do
--   letJS name value
--   withJS [name1, name2] $ \(x:x2) -> 



testVarString = "var ue_id = 'WEBRWTDQ8PSAD8KS77XA'"

test1 = parse (jsVarName >> jsString ) "" testVarString
test3 = parse (jsVarName >> jsBool ) "" "var ue_navtiming = true"
test4 = parse (jsVarName >> jsNull ) "" "var ue_navtiming = null"

between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  open
  (x, _) <- manyTill_ inside close
  pure x
 

jsVarName :: Stream s m Char => ParsecT s u m Name 
jsVarName = do
  (try $ string "let ") <|> (try $ string "var ") <|> (try $ string "const ")
  many (char ' ')
  name <- jsValidName 
  manyTill_ (char ' ') (char '=')
  many (char ' ')
  pure name


jsString :: Stream s m Char => ParsecT s u m JSString
jsString = do
  str <- between' (char '\'') (char '\'') anyChar <|> (between' (char '\"') (char '\"') anyChar) 
  pure $ JSString str

data Fractional a => JSNumber a = JSNumber a deriving (Show, Eq, Ord)  
jsNumber :: (Num a, Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSNumber a)
jsNumber = do
  whole <- some digit
  decii <- option "" $ do
    dot <- char '.'
    decimal <- some digit
    pure (dot : decimal) 

  pure . JSNumber $ read (whole <> decii)


data JSBool = JSBool Bool deriving (Show, Eq, Ord)
jsBool :: Stream s m Char => ParsecT s u m JSBool
jsBool = JSBool <$> ((True <$ string "true" ) <|> (False <$ string "false"))

data JSNull = JSNull deriving (Show, Eq, Ord)
jsNull :: Stream s m Char => ParsecT s u m JSNull
jsNull = JSNull <$ string "null"

manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go

jsValidName :: Stream s m Char => ParsecT s u m Name 
jsValidName = do
  first <- letter <|> (char '_') <|> (char '-') 
  rest <- some $ alphaNum <|> (char '_') <|> (char '-')
  pure $ first : rest

data Fractional a => JSValue a = Number (JSNumber a)
                               | String' JSString
                               | Null JSNull
                               | Boolean JSBool
             -- | Tuple [JSValue]
             -- | Array [JSValue] 

jsValue :: (Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSValue a)
jsValue = 
  (Number <$> jsNumber)
  <|> (String' <$> jsString)
  <|> (Null <$> jsNull)
  <|> (Boolean <$> jsBool)
  -- <|> jsArray
  -- <|> jsObject
  -- <|> jsTuple 



data Arg a = ArgName Name | ArgDef Name (JSValue a)
data Function a = Function [Arg a] [Dependency] [Statement]
jsFunction :: (Fractional a, Stream s m Char) => ParsecT s u m (Function a)
jsFunction = do
  args <- functionHead
  bodyStatements <- functionBody args
  
  where
    functionBody headArgs = do
      between' (char '{') (char '}') $ many (jsStatement headArgs) 
    
    functionHead = do
      string "function"
      fnName <- jsValidName
      args <- jsArgTuple
      pure (fnName, args) 
      

jsArgTuple :: (Read a, Stream s m Char) => ParsecT s u m [Arg a]
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
      

data JSStatement = JSStatement [Dependency] (Maybe Name) JS

between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a 
between1 open end match = open *> match <* end

jsStatement :: Stream s m Char => ParsecT s u m JSStatement
jsStatement = do
  mName <- optionMaybe jsVarName
  (expr, deps) <- jsExpression
  -- | either followed by ; or \n
  -- | oneOf ['\n', ';']
  pure $ JSStatment deps mName expr


parseCallsWith :: Stream s m Char => ParsecT s u m (Name, [Arg])
parseCallsWith = do
  refdName <- jsValidName
  args <- option [] jsArgTuple
  pure (refdName, args) 
  where
    
    jsArgTuple :: (Read a, Stream s m Char) => ParsecT s u m [Arg a]
    jsArgTuple = do
      char '(' *> sepBy jsExpression (char ',') <* char ')'
      
      
        -- where
        --   withDefaultArg = do
        --     name <- jsValidName
        --     manyTill_ (char ' ') (char '=')
        --     many (char ' ')
        --     v <- jsValue
        --     pure $ ArgDef name v

        --   jsArgName = ArgName <$> jsValidName

objects and functions in terms of a chain are more like interchangeable pieces where objects are just scopes

like i can do

f(1) 

unless const myobject = { f : function(x){ return x } }

then I must do:

myobject.f(1) 
  
but it can also be

myobject.g

and also objects are interesting because the question, what code do we need to run?
  1) Constructor
  2) g, or rather the .<jsName>'s
     and whatever constructs are referenced by the scopes
     2.1) I suppose you could say that for the expr: a.b.c.d:
          -> if d references a construct in c or b or a then we need it
          -> Although shitty method: can just run all of a 
  

-- Should this also be a function? 
data ObjRef = ObjRef [Name] (Maybe [Arg])


-- as this recurses, we will need to add dependencies 
jsExpression = do
  jsValue
  
  <|> existingObjRef
  <|> jsFunction
  <|> functionApplication 

  <|> jsValidName -- IS REF 
  -- | COMPLICATION:
    -- WE CAN ALWAYS JUST DO .something so this is like a chain on the returned props which is
    -- gonna be another `parseCallsWith` 

  where

    functionApplication :: Stream s m Char => ParsecT s u m FunctionRef
    functionApplication = parseCallsWith 

    existingObjRef :: Stream s m Char => ParsecT s u m ObjRef
    existingObjRef = do
      jsValidName -- if it's a name, we know that it must be a reference
      many (char '.' >> jsValidName)
      between1 (char '(') (char ')') $ jsExpression 

    -- has a dependency by default 
    objInstantiation = do
      string "new"
      many (char ' ')
      nameRefd <- jsValidName
      args <- jsArgTuple 
      pure (nameRefd, args)
      
      
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
execJS :: MonadJS m => [JSStatement] -> m () 
execJS = do undefined
  
  -- |case js of
     -- Runnable -> do runJSWithCli =<< (substitute js) <$> get 
     -- NotRunnable -> put to state
     -- Iterator -> update state where state :: Map Name JSValue
     -- LocationChange -> undefined
     --   -- this JS Context is now Void except Window object 


---------HIGHLIGHTS - Mayve Compilable ?--------------------------------------------------------------------------

data Statement = Statement [Dependency] (Maybe Name) JS
data JSStatement' = WhileLoop [Dependency] RawJS
     		 | ForLoop [Dependency] RawJS -- note that multiple 'lets' could be used
		 | CaseStatement (Map Condition RawJS) -- would also need ?: syntax 
		 | Function' Name RawJS
		 | ObjectDeclaration Name RawJS
		 | Exec JSExpression -- in current top level as raw statement, no assignment
		 | VarAssign Name JSExpression -- this could be an empty expression 
		 | Iterator 

type RawJS = JS

type Dependency = Name

data Condition = Condition RawJS

-- this is derived from stream editing 
data JSExpression = JSExpr [Dependency] RawJS 


data JSValue' = JSValue' JSStatement [Update]

data Update = Reassign JSExpression
     	    | Iterate JSStatement 


data ExprPiece = ExprPiece String 
-- jsSetValue :: ParsecT s u m ((Name, [Dependency], [ExprPiece])) 
-- jsSetValue = do
--    manyLeftSide
--    maybeParse (assignmentOperator >> expression) 

--    where assignmentOperator = char '='
--                             <|> (otherOperator >> (char '='))

data JSString = JSString String deriving (Show, Eq, Ord)
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
