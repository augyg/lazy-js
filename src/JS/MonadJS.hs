{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module JS.MonadJS where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Exception
import Control.Monad (liftM)
import Data.Aeson 
import qualified Data.Map as M
import Data.Text 

import JS.Source
import JS.Types 


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

class (MonadIO m, Monad m) => MonadJS m where
  runJS' :: JS -> m () -- Update is in state 
  getAST :: m (FullAST a)
  putAST :: (FullAST a) -> m ()
  putNewEntryAST :: [Name] -> ExprAST a -> m ()
  modifyEntryAST :: [Name] -> ExprAST a -> m ()
  enterFunction :: m () -- todo: will also have merge 'this' logic with normal cases
  exitFunction :: m ()  -- todo: will also have merge 'this' logic with normal cases
  --setAST :: Name -> String -> m ()
  {-# MINIMAL getAST, putAST #-}



class HasJSState s where
  getRef :: (Fractional a, Read a) => s -> ExprAST a
  -- TODO: this 
  putRef :: (Fractional a, Read a) => s -> Name -> ExprAST a -> ExprAST a
  -- TODO: 
  --modifyRef 

  
  
-- instance HasJSState (MyAST, [MyAST], JSAST)
-- instance HasJSState ([MyAST], JSAST)
-- instance HasJSState JSAST 

--instance HasJSState (Maybe (MyAST a), [MyAST a], JSAST a)

--instance MonadJSState state => MonadJS (JST state) where
  

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

-- -- | TL;DR: This is a way to build a deterministic outSet
-- -- | ... which we can build with a Map of key values from the last run
-- data JSAST' = JSAST' { inSet :: M.Map Name JSVal -- could include dom = ... 
--                      -- , outSet :: [Name]
--                      , script' :: JS
--                      -- ^ FunctionDeclarations data type is really a subcategory of this
--                      -- ^ this just is meant to rep that as well as setting variables
--                      -- ^ although we could run function declaractions
--                      -- ^ or make a Module data structure 
--                      }







-----------------------------------------------------------------------------
-- | In order for a
-- type JSAST = JSRecordC
-- type MyAST = JSAST
type FullAST a = (Maybe (MyAST a), [MyAST a], JSAST a)
type JSError = String
newtype JST num m a = JST { runJST :: ExceptT JSError (StateT (FullAST num) m) a }
  deriving (Functor, Applicative, Monad, MonadException)


-- --\s -> Either Error (



instance MonadTrans (JST num) where
  --lift :: Monad m => m a -> t m a
  lift m = JST $ ExceptT $ StateT $ \s -> do
    a <- m
    pure $ (Right a, s)

  -- lift m = JST $ StateT $ \s -> ExceptT $ do
  --   a <- m
  --   pure $ Right (a, s)

-- instance MonadJS (JST num m) where
--   getAST = 
  



-- | TODO(Galen): add in forall m. MonadIO m => m
newtype JSDomT m a = JSDomT { runJSDomT :: StateT (Html, JSAST a) (ExceptT JSError m) a }

--newtype MonadJS' a = MonadJS' a

-- | We could also just only parse when we need to get back into the haskell context
-- | I dont know if I need the reader part
newtype MonadJST m a = MonadJST { runMonadJS :: StateT (JSAST a) m a } 
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- data Fail = Fail { url :: Link
--                  , html :: Html
--                  } deriving (Show) --- , Generic)
-----------------------------------------------------------------------------


