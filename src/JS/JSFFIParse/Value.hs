{-# LANGUAGE FlexibleContexts #-} 

module JS.JSFFIParse.Value where

import JS.JSFFIParse.Combinators (between')

import Text.Parsec
import Control.Applicative (some)

{-
   TODO:
     Simplify typification contained in JSValue
-}


data Fractional a => JSValue a = Number (JSNumber a)
                               | String' JSString
                               | Null JSNull
                               | Boolean JSBool
             -- | Tuple [JSValue]
             -- | Array [JSValue] 

data JSString = JSString String deriving (Show, Eq, Ord)
data Fractional a => JSNumber a = JSNumber a deriving (Show, Eq, Ord)  
data JSBool = JSBool Bool deriving (Show, Eq, Ord)
data JSNull = JSNull deriving (Show, Eq, Ord)


jsValue :: (Read a, Fractional a, Stream s m Char) => ParsecT s u m (JSValue a)
jsValue = 
  (Number <$> jsNumber)
  <|> (String' <$> jsString)
  <|> (Null <$> jsNull)
  <|> (Boolean <$> jsBool)
  -- <|> jsArray -- NOTE: this can be lazy cuz the parser will be lazy 
  -- <|> jsObject
  -- <|> jsTuple 




jsString :: Stream s m Char => ParsecT s u m JSString
jsString = do
  str <- between' (char '\'') (char '\'') anyChar <|> (between' (char '\"') (char '\"') anyChar) 
  pure $ JSString str


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


