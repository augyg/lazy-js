{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-} 

module JS.JSFFIParse.Combinators where


import Text.Parsec
import Control.Applicative (liftA2)

-- do
--   letJS name value
--   withJS [name1, name2] $ \(x:x2) -> 

between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a 
between1 open end match = open *> match <* end



manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


sspace :: Stream s m Char => ParsecT s u m [Char]
sspace = many (char ' ') 


inSpace :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inSpace p = sspace *> p <* sspace


between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  open
  (x, _) <- manyTill_ inside close
  pure x
 
