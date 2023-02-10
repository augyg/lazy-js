module JS.Types where

import Data.These
import JS.Source
import qualified Data.Map as M

type JSVal = String -- todo: Text
type Name = String
type NameChain = [Name]
type BaseUrl = Url
type Url = String

--data JSType = JSONString String | JSNumber String | JSString String






---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

type RawJS = JS

type Dependency = Name

type Condition a = Expr a
--data Condition = Condition RawJS

-- | TODO: remove Fractional -> Scientific 
data Fractional a => JSNumber a = JSNumber a deriving (Show, Eq, Ord)  
data JSBool = JSBool Bool deriving (Show, Eq, Ord)
data JSNull = JSNull deriving (Show, Eq, Ord)
data JSArray a = JSArray [Expr a] deriving (Show, Eq, Ord) 
data JSString' = JSString String  deriving (Show, Eq, Ord) 

data Fractional a => JSValue a = Number (JSNumber a)
                               | String' JSString'
                               | Null JSNull
                               | Boolean JSBool
                               | Array (JSArray a)
                               | Record (JSObject a)
                               | JSUndefined
                               | NaN
                               deriving (Show, Eq, Ord) 



  -- | Because of being an object
-- | TODO(galen): NOTE: for a: x += 1 or  a: x = 1  console.log (<obj>.a) --> x+1 or 1
data JSObject a = JSObject [(Name, Expr a)] deriving (Show, Eq, Ord)

-- | TODO(galen): VALID:  var xn = { f() { return 1}, g() {}, a:1 }
data Method a = Method Name [ArgName a] [JSTopLevel a] deriving (Show, Eq, Ord)


-- data JSObject a = JSObject [(Name, Expr a)] deriving (Show, Eq, Ord) 
-- data JSObject a = JSObject [(Name, ObjectOriented a)] deriving (Show, Eq, Ord)


type Constructor = Method
type Extends = Dependency
data JSClass a = JSClass (Maybe Name) (Maybe Extends) (Maybe (Constructor a)) [Method a] deriving (Show, Eq, Ord)


type InFnScope = Bool


  
data JSTopLevel a = Control' (Control a)
                  | Declare (ObjectOriented a)
                  | Return (Expr a) -- Weird case --TODO
                  | Break -- another weird case
                  deriving (Show, Eq, Ord)
-- throw, return, break 
                  
data Control a = While (WhileLoop a)
               | For (ForLoop a){-todo:(ForLoop a) -}
               | IF (IFStatement a)
               | Switch (SwitchStatement a)
               | TryExcept (TryExceptFinally a)
               deriving (Show, Eq, Ord)
               -- | Return 

data ObjectOriented a = Function' (Function a)
                      | Class (JSClass a)
                      | Operation (JSOperation a) -- TODO make this singular again
                      deriving (Show, Eq, Ord)


-- | TODO(galen): Technically the 2nd datatype of ForHead should be expressions seperated
type ForHead a = (Maybe [JSOperation a], (Condition a, [Dependency]), Maybe [JSOperation a]) 
data ForLoop a = ForLoop (ForHead a) [JSTopLevel a] deriving (Show, Eq, Ord)

data IFStatement a = IFStatement [((Condition a, [Dependency]), [JSTopLevel a])] deriving (Show, Eq, Ord)

type DefaultCase a = Maybe [JSTopLevel a] 
data SwitchStatement a = SwitchStatement (Expr a, [Dependency]) [(JSValue a, [JSTopLevel a])] (DefaultCase a)
  deriving (Show, Eq, Ord)

-- | Must be run with runJSWithCliErr (although we might as well use this for all) 
-- | IMPORTANTE!!! All try excepts should be strict for the try block
type TryBlock a = [JSTopLevel a]
type Catch a = (Maybe Dependency, [JSTopLevel a]) -- dependency is the err name
type Finally a = [JSTopLevel a]
type CatchFinally a = These (Catch a) (Finally a)
data TryExceptFinally a = TryExceptFinally (TryBlock a) (CatchFinally a) deriving (Show, Eq, Ord)


data WhileLoop a = WhileLoop (Condition a, [Dependency]) [JSTopLevel a] deriving (Show, Eq, Ord)




-- | Note: If a function is called to set some variable 'y' and that function performs an 'internal'-side-effect
-- | on some variable 'x', if we run the operation 'let y = somefunc()' then x will receive the effect at this time
-- Anything which can be named 
data Expr a = Val (JSValue a)
            | Reference (Ref a) -- decl. function app and/or object reference
            | FuncAsExpr (Function a)
            | ClassAsExpr (JSClass a) 
            | ApplyFunc (Either Name (Function a)) [Expr a] -- inline funcs + ones in the AST
            | New Name [Expr a]
            -- | TODO: New (Expr a) [Expr a] because: new {a : class {} }.a(1)
            -- | and: new (class A {})()
            | Op Operator (Expr a) (Expr a)
            --  This Chain -- valid globally
            -- SHOULD WE HAVE NEXT? 
            -- | PureOp Operator (Expr a) (Expr a)
            -- the only thing that can definitively be pure is a PureOp where we have purified
            -- (this technically means that we could make all functions pure, but only at certain points
            -- and so therefore why would we keep it as a function - which represents an Op + SideEffects on
            -- global state?) 
            deriving (Show, Eq, Ord)
            -- var a = function f(x) { ... }
            -- note that f is not defined 


-- | We could eval with a trick
-- | return x --> let returned = x --> (runChained returned : returned.f() ) 
data Ref a = Ref [Name] [Dependency] (Maybe (RefArgs' a)) (Maybe (Ref a)) deriving (Show,Eq, Ord)
type RefArgs' a = [Expr a] -- cuz we've extracted the deps for the ref 



-- | For eval of AssignOp,
-- | state1 >>= \x -> assignF y x
-- | except for A_Equal : state1 >>= \_ -> y
-- |   where y :: Expr
-- |
-- | ACTUALLY MAYBE: we can just check where 
data AssignOp = A_Equal -- set to 
              | A_Plus
              | A_Subtract
              | A_Multiply
              | A_Divide
              | A_Modulo
              | A_Exponentiation
              | A_LShift -- <<=
              | A_RShift -- >>= (JS)
              | A_UnsignedRShift
              | A_ANDB
              | A_XORB --- ^=
              | A_ORB
              | A_AND
              | A_OR
              | A_Nullish
              deriving (Show, Eq, Ord)


-- Note that these can literally pair with any two JS Values 
data Operator = Multiply
              | Plus
              | Subtract
              | Exponentiation
              | Division
              | Modulo
              | Equal -- (==)
              | EqualSameType  -- (===)
              | NotEqual
              | NotEqualOrNotSameType
              | GreaterThan
              | LessThan
              | LessOrEqual
              | GreaterOrEqual
              | Ternary -- ? ~~ isTrue
              | AND
              | OR
              | NOT
              | ANDB
              | ORB
              | NOTB
              | XORB
              | Shift String
              deriving (Show, Eq, Ord)



-- | We know that the first 3 cases will be the A_Equal op if we were to bind AssignOp to these cases as well
-- | since that's only legal
data VarDecl = Let Name
             | Var Name
             | Const Name
             | Raw AssignOp [Name]
             deriving (Show, Eq, Ord)
-- | And I could still use Maybe to represent Raw Expressions
  -- Just  --> Updates AST
  -- Nothing --> 'run's but no variable set/assigned (may perform side effects --> (globals | IO) )
      -- NOTE: if there's no deps for a function (if we track) then we can see if local AST is affected upfront
      -- But why would that ever exist?

-- | TODO: z = t = u = 1
data JSOperation a = JSOperation [Dependency] (Maybe VarDecl) (Expr a) deriving (Show, Eq, Ord)



-- | Inner AST for functions 
type MyAST a = JSAST a

--type JSAST a = JSRecordC a
type JSAST a = GenericObject a 




-- class HasJSState s where
--   getRef :: (Fractional a, Read a) => s -> ExprAST a
--   -- TODO: this 
--   putRef :: (Fractional a, Read a) => s -> Name -> ExprAST a -> ExprAST a
--   -- TODO: 
--   --modifyRef 
 
  
-- instance HasJSState (MyAST a, [MyAST a], JSAST a)
-- instance HasJSState ([MyAST a], JSAST a)
-- instance HasJSState (JSAST a)
  
  


-- Output of smart constructor which will use isUnaffectedByState
data JSValueC a = NumberC (JSNumber a)
                | StringC JSString' 
                | NullC JSNull
                | BooleanC JSBool
                | ArrayC (JSArrayC a)
                | RecordC (GenericObject a)
                | JSUndefinedC
                deriving (Eq, Ord, Show)
                
data JSArrayC a = JSArrayC [ExprAST a] deriving (Eq, Ord, Show)
-- data JSRecordC a = JSRecordC [(Name, ExprAST a)]

-- | Note: when we add key and index lookups, there wont be a direct equivalent ExprAST for that Expr a
-- | Note: this is very similar to what we had for ObjectOriented 
data ExprAST a = ValC (JSValueC a)
               -- | FuncAST (Function a)
               | ClassAST (JSClass a)
               | PureOp Operator (ExprAST a) (ExprAST a)
               deriving (Eq, Ord, Show)


-- | Arg is just meant to represent names for special scopes, not args passed in a statement
data ArgName a = ArgName Name | ArgDef Name (Expr a) deriving (Show, Eq, Ord)
--data Function a = Function (Maybe Name) [ArgName a] [Dependency] [JSStatement] (Maybe (Return a))
data Function a = Function (Maybe Name) [ArgName a] [JSTopLevel a] deriving (Show, Eq, Ord) 
-- | Can only put to AST if we have a name for it 


-- | The raw core of what a function is 
data Lambda a = Lambda [ArgName a] [JSTopLevel a] deriving (Eq, Ord, Show)                
data GenericObject a = GenericObject (Maybe (Lambda a)) (M.Map Name (ExprAST a)) deriving (Eq, Ord, Show)

