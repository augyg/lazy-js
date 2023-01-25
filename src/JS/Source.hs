{-# LANGUAGE OverloadedStrings #-}

module JS.Source where

import Data.Text
import qualified Data.Map as M

type Attributes = M.Map String String
data Script = Script Attributes Text deriving (Show, Eq) 


-- | Must not start with let 
newtype JSExpr = JSExpr { express :: Text }


instance Semigroup Script where
  (Script _ b) <> (Script _ b2) = (Script mempty (b <> ";\n" <> b2))

instance Monoid Script where
  mempty = Script mempty mempty 

type RawJS = JS
newtype JS = JS { unJS :: Text }

instance Semigroup JS where
  JS script <> JS runNext = JS $ script <> ";\n" <> runNext 

instance Monoid JS where
  mempty = JS ""
