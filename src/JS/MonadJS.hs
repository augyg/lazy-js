module JS.MonadJS where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson 
import qualified Data.Map as M
import Data.Text 

import JS.JSFFIParse.Expr
import JS.Source
import JS.Types (JSVal, Name)



-- faked, not really in use yet
import Control.Monad.Trans.Writer 


{-

Idea:
  regarding functions and generally things which hold multiple javascript statements


  we could just note that these N variables have been affected from this <scope> being called

  so if in the course of the function for global variable t we say:

  t += 1

  We add this to the stack held in t

  and only when we need t for some reason (doesn't have to necessarily be side effects but some particular
  statement (we could force some expressions to be strict)(maybe we also keep track of tree size and
  fold ops when we reach a certain threshold)(even just by forcing the execution of all new commands) 

  This also means I will need to have the concept of a dependency denote the Nth step

  Although it may be ridiculously memory heavy to keep track of all steps in case we want to use one based

  on an old value. We can deref this tho by saying

  "X depends on Y4 and Z depends on Y7" -> SO compute Y4 -> Compute X, Then continue (and do the same for Z
  if another value depends on Y11"

  Although I wonder if we could do something like combine Ops so that we delete all the steps that are never asked

  like the script:

  let x = 3
  let z = x

  x +=1 -- x2
  x +=2 -- x3 

  x += 10 --x4

  let y = x -- x5

  So only x1 and x5 are subscribed -> therefore, could we not merge 2->5

-}



type Html = String 

class MonadJS m where
  runJS' :: JS -> m () -- Update is in state 
  getAST :: m JSAST
  putAST :: JSAST -> m ()
  setsAST :: Name -> String -> m ()
  {-# MINIMAL getAST, putAST #-}

getsAST :: (MonadJS m, FromJSON a) => Name -> m a
getsAST name = undefined

-- setsAST :: MonadJS => Name -> JSValue -> m () 
-- setsAST name val = undefined
-- -- | Bool if String
-- setJSVal :: Name -> JSVal -> JS 
-- setJSVal jsName jsVal = JS $ "let " <> (pack jsName) <> " = " <> (showJSType . f' $ jsVal) <> ";"


-- | Declare and save what you want to carry from last run 
orderJS :: MonadJS m => M.Map String String -> m ()
orderJS = undefined

-- | delete keys
resetSomeJS :: MonadJS m => M.Map String String -> m ()
resetSomeJS = undefined

-- | TL;DR: This is a way to build a deterministic outSet
-- | ... which we can build with a Map of key values from the last run
data JSAST = JSAST { inSet :: M.Map Name JSVal -- could include dom = ... 
                   -- , outSet :: [Name]
                   , script' :: JS
                   -- ^ FunctionDeclarations data type is really a subcategory of this
                   -- ^ this just is meant to rep that as well as setting variables
                   -- ^ although we could run function declaractions
                   -- ^ or make a Module data structure 
                   }


-----------------------------------------------------------------------------
-- | In order for a 
newtype JST m a = JST { runJST :: StateT JSAST (ExceptT JSError m) a }

-- | TODO(Galen): add in forall m. MonadIO m => m
newtype JSDomT m a = JSDomT { runJSDomT :: StateT (Html, JSAST) (ExceptT JSError m) a }

--newtype MonadJS' a = MonadJS' a

-- | We could also just only parse when we need to get back into the haskell context
-- | I dont know if I need the reader part
newtype MonadJST m a = MonadJST { runMonadJS :: StateT JSAST m a } 
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
type JSError = String
-- data Fail = Fail { url :: Link
--                  , html :: Html
--                  } deriving (Show) --- , Generic)
-----------------------------------------------------------------------------



-- | JSTopLevel datatype implies:
data Script' a = Script' [JSTopLevel a]
-- | Which thus implies maybe I should just make the Num a => a, a Float
--- AND
-- | This implies: we could do for space efficiency:
data ScriptRaw = ScriptRaw [(Dependency, RawJS)] -- where type RawJS = JS
-- | AND implies:
newtype ScriptWriter num m a = ScriptWriter { unScriptWriter :: WriterT (Script' num) m a }
-- runScriptWriter has access to a node path
