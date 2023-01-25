module JS.JSFFIParse.Operation where

import JS.JSFFIParse.Expr
import JS.Types (Name)

import Text.Parsec


-- | This is an Expression which may be named
-- | TODO(galen): iterators
-- | TODO(galen): obj.newField = <VAL>
data JSOperation a = JSOperation [Dependency] (Maybe Name) (Expr a) --JS
jsOperation :: (Fractional a, Read a, Stream s m Char) => dependencies  -> ParsecT s u m (JSOperation a)
jsOperation dependencies = do
  mName <- optionMaybe jsVarName -- todo: assignment operators -- TODO multiple vars per var keyword
  (expr, deps) <- jsExpression  
  -- | Either followed by ; or \n
  -- | oneOf ['\n', ';']
  pure $ JSOperation deps mName expr


-- | Is an Operation as expressed to an expression because of 
jsIterator :: Stream s m Char => ParsecT s u m (JSOperation a)
jsIterator = undefined
  -- should include both
  -- i++
  -- i+=<VAL>


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
  rest <- some $ alphaNum <|> (char '_') <|> (char '-')
  pure $ first : rest


  
