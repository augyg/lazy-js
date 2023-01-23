module JS.ScrapeNA where

-- | Everything here is no longer in use and we are trying to move away from this model


-- | If we can parse JSON (which we can) then we have Objects
-- | For now though, I will just takeTill ;
-- | and then reset 
jsSetVariable :: [(Name, JSVal)] -> ScraperT Name 
jsSetVariable setters = do
  name <- jsVarName
  when (not $ elem name $ fmap fst setters) parserZero 
  (value,_) <- manyTill_ anyChar (char ';')
  pure name
  --_ <- (JSON <$ object)
  --     <|> 
  --where
  --  object = char '{' 

overrideScript :: [(Name, JSVal)] -> JS -> JS 
overrideScript overrides (JS js) = JS $ pack $ streamEdit (jsSetVariable overrides) (f $ overrides) (unpack js)
  where 
    -- | Temporarily String 
    f :: [(Name, JSVal)] -> Name -> String
    f ast name = let v = fromJust $ lookup name ast
                 in unpack $ unJS $ setJSVal name v 
findJSVal :: ParsecT s u m a -> [JSVal] -> a
findJSVal = undefined


f' :: JSVal -> JSType 
f' val
  | length (filter (== '{') val) == length (filter (== '}') val) = JSONString val
  | exists (oneOf (['A'..'Z'] <> ['a'..'z'])) val = JSString val
  | length val == (length $ filter (\x -> elem x ['0'..'9']) val) = JSNumber val  
  | otherwise = error "unknown javascript data type" 


-- | Bool if String
setJSVal :: Name -> JSVal -> JS 
setJSVal jsName jsVal = JS $ "let " <> (pack jsName) <> " = " <> (showJSType . f' $ jsVal) <> ";"
