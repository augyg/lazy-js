module JS.MonadJS where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson 
import qualified Data.Map as M
import Data.Text 

import JS.Source
import JS.Types (JSVal, Name)


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


