{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module JS.JS where



import JS.MonadJS
import JS.Source
import JS.Types


import Control.Monad.Trans.Except
import Control.Applicative (some, liftA2)

import qualified Data.Map as M
import Text.Parsec
import Data.Text (Text, pack, unpack)

-- | Likely will make this module functions like with AST do some shit 

-- import Scrappy.Requests (getHtml, getHtml', SessionState(..))
-- import Scrappy.Scrape (ScraperT, scrape, exists)
-- import Scrappy.Elem.SimpleElemParser (el, manyTill_)
-- import Scrappy.Elem.Types (Elem'(..))
-- import Scrappy.Links (BaseUrl, Src, Link(..), fixRelativeUrl, renderLink, LastUrl, Html)
-- import Scrappy.Find

-- import Language.Haskell.TH (recover)
-- import System.Which (staticWhichNix, staticWhich)
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import System.Process (CreateProcess(..), proc, readCreateProcess)
-- import System.IO.Temp (withTempFile)
-- import System.IO (hFlush)
-- import Data.Text.IO (hPutStr)

-- import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
-- import Control.Monad.Trans.State (StateT)
-- import Control.Applicative (some, optional)
-- import Text.Parsec (ParsecT(..), Stream, ParseError, sepBy, char, many, anyChar, parse, noneOf, string, manyTill
--                    , try, alphaNum, (<|>), oneOf, parserZero)
-- import Control.Monad (when)
-- import Control.Applicative (some)
-- import Data.Either (fromRight)
-- import Data.Maybe (fromMaybe, fromJust, catMaybes )
-- import qualified Data.Set as S

-- import Data.Text (Text, pack, unpack)
--import Data.List.Extra (isInfixOf)
--import Data.ByteString.Lazy (ByteString)
--import Data.String.Utils (strip)




----- TOO HIGH LEVEL --------------------------------------------------------------------------------------------

-- -- This is fine cuz I'll probably eliminate Attrs field in script 
-- toScript' :: SessionState sv => sv -> LastUrl -> Src -> String -> IO Script
-- toScript' sv (Link baseU) src inner
--   | null inner && (isInfixOf baseU src) = getHtmlST sv (Link src) >>= (\(scriptSrc, _) -> pure $ Script mempty $ pack scriptSrc) 
--   | null inner && (src /= "") = do
--       -- getHtml with rectified URL then set as body
--       let url = fixRelativeUrl (Link baseU) src
--       (scriptSrc,_) <- getHtmlST sv (Link url)
--       pure $ Script mempty $ pack inner
--   | otherwise = pure $ Script mempty $ pack inner


----- TOO HIGH LEVEL --------------------------------------------------------------------------------------------

-- Heres the solution:

  
--   I stream edit the collection of JS scripts (so first I concat them)

--   I stream edit, changing parsed DOMRefs into expressions that are effectively
--   fancy rewirings of \spec -> scrape elemParser spec

--type Html = String 


manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go




type Body = Text
--type JSVal = String -- todo: Text


-- | In how we write the script, there is no reason that we need to 
data JSInteraction = Get Name
                   | GetByExpr Name JSExpr
                   -- ^ Name is new name 
                   | Set Name JSVal
                   | SetGet Name JSVal
                   -- ^ Can be any name 


type Attributes = M.Map String String

-- | All this is, is a signal to me for what to console.log
-- | 
data JSCall = JSCall JS ReturnVars 

type ReturnVars = [Text]


newtype FunctionDeclarations = FuncDecls { decls :: Text }




--type Name = String 


type JSFunc = JSVal -> JSExpr
-- type JSFunc' a b = a -> MonadJS b 



-- 1)
exampleJSFunc :: JSVal -> JSExpr
exampleJSFunc jsval = JSExpr $ pack jsval 


fromJSFunc :: JSFunc 
           -> Name
           -> JS
          -- ^ this could have 'a' as a phantom type that enforces type on the return into haskell
          -- but that would also disallow (a -> b) and only (a -> a)
          -- But this also means that this should be a Monad to be a full fledged FFI 
fromJSFunc jsExpr name = JS $ 
                       pack $ "let " <> name <> "=" <> (unpack $ express $ jsExpr name) <> ";\n"
                       <> "console.log(" <> name <> ");"
  



-- showJSType :: JSType -> Text 
-- showJSType (JSONString str) = pack str
-- showJSType (JSNumber str) = pack str
-- showJSType (JSString str) = "\"" <> (pack str) <> "\"" 

mkJSDOMAct :: Html -> JS
mkJSDOMAct indexHtml = undefined

unScript :: Script -> Text
unScript (Script _ s) = s



                   
      
--      <> "});"



-- | Using a JS Source that may have some functions, run some command at the end which uses these scripts
-- | This means any function declarations in the Script are in scope for command
withScript :: Script -> JS -> Script 
withScript (Script a script) (JS command) = Script a (script <> ";\n"
                                                      <> "console.log(" <> command <> ")")



-- | This will be collected into the map 
getJSVal :: Name -> JS 
getJSVal jsName = JS $ "console.log(`" <> (pack jsName) <> "`, " <> (pack jsName) <> ");"

getJSVal' :: MonadJS m => Name -> m JSVal
getJSVal' = undefined

-- NOTE: remember to test Higher Kinded datatypes that are semigroup-ish





filterSets :: JSInteraction -> Maybe JS
filterSets (Get name) = Nothing
filterSets (Set name value) = Just $ setJSVal' name value where setJSVal' = undefined -- original in ScrapeNA
filterSets (SetGet name value) = Just $ setJSVal' name value where setJSVal' = undefined -- original in ScrapeNA
filterSets (GetByExpr _ _) = Nothing 

subscribe :: MonadJS m => Name -> m a
subscribe = undefined

-- | When STDOUT writes the results, they are separated by \n char
filterGets :: JSInteraction -> Maybe JS
filterGets (Set _ _) = Nothing
filterGets (Get name) = Just $ getJSVal name
filterGets (SetGet name val) = Just $ getJSVal name
filterGets (GetByExpr newName exprJS) = Just . JS $ "console.log(\"newName:\", (" <> (express exprJS) <> "));"

-- | TODO(galen): set infix to be lower than <>
-- | TODO(galen): check if last character of a is ; 
(<&>) :: JS -> JS -> JS
(<&>) (JS a) (JS b) = JS $ a <> ";\n" <> b


-- | Specialized to only taking the last n and put them in the proper order 
takeJSOut :: Int -> [JSVal] -> [JSVal]
takeJSOut n vals = reverse $ take n vals 


-- | IDEA !!!!

 --  to extend to references from the DOM such as an onclick, I can literally just append this to runVDOMWith $ JS domRef

  -- <div onclick="f()"> --> runVDOMWith dom (JS $ getOnClick divElem) 

  --  I can probably even do this in a safe manner where this only needs
  --  the run script option of "external" or whatevs in JSDOM







--- toScript is not working

-- -- | I wouldnt need this at all tho for 
-- toScript :: SessionState sv => sv -> BaseUrl -> Elem' a -> IO (Maybe Script)
-- toScript mgr baseUrl (Elem' _ atrs _ inner) = do
--   let src = M.lookup "src" atrs
--   case src of
--     Just a -> Just <$> toScript' mgr baseUrl (fromMaybe "" src) (strip inner)
--     Nothing -> pure Nothing 
-- --  print src


-- could do:

--   fetch normally
--   when (not . exists $ target) $ do runVDOMFetch lastUrl
--      when this == none $ do endsite 


--getHtmlPromise :: Link -> Html


--           writeFile url html
--           writeFile (url <> "2") htmlV

-- someFunc :: Html -> Maybe a

        








      


  -- case (null inner && (src /= Nothing)) of
  --   True -> do
  --     -- check if src is relative or absolute
  --     if 
  --     (scriptBody, _) <- getHtmlST mgr $ fromJust src
  --     pure $ Script atrs $ pack scriptBody 
  --   False ->
  --     pure $ Script atrs $ pack inner

-- getHtmlRaw = getHtmlST

-- getHtmlFinal :: SessionState sv => sv -> String -> IO Text
-- getHtmlFinal mgr url = do
--   (html, _) <- getHtmlST mgr url
--   -- print html
--   let
--     Just scriptElems = scrape (el "script" []) html
--     -- scripts' = scripts >>= (\ss -> fmap toScript ss)
--   scripts <- mapM (toScript mgr) scriptElems
--   let
--     Script _ firstScript = head scripts
--     Script _ theScript = mconcat scripts 
--   runScript firstScript--theScript
  
  
-- data HTML = Raw Html | TreeH (Forest HTML) -- theoretically speaking  



-- mkSession :: IO Session
-- mkSession = newSession defaultConfig 

-- -- | Doesnt work at all 
-- doJS :: IO ()
-- doJS = do
--   sesh <- mkSession
--   print "got here"
--   (result :: ByteString) <- eval sesh [js| 1+1; |]
    
--   print result
--   pure ()



 -- -get node path from environment (set by nix prior)
 -- -withTempFile run script in node and redirect standard output back into local Monad
 -- -output == document.body ->then: decode into haskell String ~ Html

 --  **** AS a test though:; 1 + 1 ;;; function () { return 1; } ;;; then above 3rd step 




-- getNodePath :: 


-- -- | Nice pattern : get environment variable and execute the specified command   
-- procCli :: FilePath -> [String] -> CreateProcess
-- procCli workingDirectory args =
--   addEnvironmentVariable ("CKB_CLI_HOME", workingDirectory) $ proc ckbCliPath args
--   -- ::  (String, String) -> CreateProcess -> CreateProcess
  
-- | Should not be needed with static which ? 
-- addEnvironmentVariables :: [(String, String)] -> CreateProcess -> CreateProcess
-- addEnvironmentVariables args cp =
  -- cp { env = env cp <> Just args }

  

-- | To avoid file collisions I could create a hash that the file is named with






-- f = do
--   -- req <- parseRequest "https://hoogle.haskell.org"
--   -- mgr <- newManager tlsManagerSettings
--   download  
--   readCreateProcess (proc nodePath ["index.js"]) []
--   where
--     download = (writeFile "index.js") . unpack . unScript . runVirtualDOM . pack =<< getHtml' "https://hoogle.haskell.org" 


-- test = do
--   mgr <- newManager tlsManagerSettings
--   let baseUrl = "https://hoogle.haskell.org"
--   (indexHtml, _) <- getHtmlST mgr baseUrl 
--   let Just scripts = scrape (el "script" []) indexHtml
--   scripts' <- mapM (toScript mgr baseUrl) scripts
--   -- x <- runJSWithCli $ Script mempty "console.log('hello world');\nconsole.log('hello again')"
--   --print "head"
--   --print $ head scripts'
--   let finalScript = mconcat scripts'
--   -- print finalScript 
--   x <- runJSWithCli finalScript
--   print "ran script"
--   -- print $ take 5 x
--   pure ()


-- it is clear that in order for us to get values back into haskell they have to first be directed to stdout
-- so if we were to do any execution, it would be pointless if that were not wrapped with console.log() or
-- the results or variables were not redirected to some JSON file/struct 


-- buildJSCall :: 




-- data JSCall' = JSCall' (Interface a b) (a -> b) 



  

-- | runVDOMWith dom $ JS $ "let x=1" <> "console.log(x)"


-- IDEA!!!
-- | haskellFunctionThatMakesJS arg1 arg2 arg3 = writeJSFunc arg1 arg2 arg3 ~~~ "let x=1; y=3; v=4; function f(x,y,v) { logic -> return o }; console.log(f(x,y,v))" 

-- | One way that we could actually do this, is by making the implementor write the logic of the function for javascript
-- | 

-- | f :: JSFunc -> A -> B -> C -> JS 
-- | f jsFunc a b c = undefined

-- IDEA!!!! WE could also use the outputted JSVals to create a recursive interface
                                 -- that creates DOM (but why would I?) 



-- -- | Would be really cool if we could somehow state the return type
-- -- | And i feel like we can:
-- -- |
-- -- |  Script a = Script Text -- where a is a Phantom type that must be parsed into
-- -- |
-- -- | further :     Script (Maybe a) = Script Text --- > Conceptually at least
-- -- |
-- mkVDOMScript :: Text -> Script
-- mkVDOMScript htmlTmpFilePath = Script mempty
-- --                     $ "let html= Buffer.from(`" <> (pack . (filter (\x -> (x /= '\n') || (x /= '\t'))) . unpack $ html) <> "`, 'utf8');"
-- --                     $ "let html= Buffer.from(, 'utf8');"
--                      $ "let fs = require('fs');"
--                      <> "var text = fs.readFileSync(\"" <> htmlTmpFilePath <> "\").toString('utf-8');"
--                      <> "console.log(text);"
--                      -- <> "let jsdom = require('jsdom');"
--                      -- <> "let dom = new jsdom.JSDOM(html);"
--                      -- <> "console.log(dom.serialize())"







-- withTempFile "." "index.html" $ \htmlFPath handle -> do
--   hPutStr handle $ pack htmlString
--   hFlush handle
--   runJSWithCli $ mkVDOMScript htmlFPath
--   where
--     mkVDOMScript fp = Script mempty
--                      $  "let fs = require('fs');"
--                      <> "let jsdom = require('jsdom');" 
--                      <> "var text = fs.readFileSync(\"" <> (pack $ filter (\x -> (x /= '\n') || (x /= '\t')) $ fp) <> "\").toString('utf-8');"
--                      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\" });"
--                      <> "console.log(dom.serialize());"
  

-- newtype VirtualT m a = StateT DOM m a 


-- type DOM = (Html, JSAST) 



-- for this to work properly I need to write a JS template that takes as input the prevous state and applies
-- some js to it like the 3 main cases

-- \x -> x -- just grab it
-- \_ -> 1 -- just set it
-- \x -> x + 1 -- set it with itself as input for the output 





-- Writing the state of our map representing JSVal's is just set at the beginning

  
-- writeAST :: Map String String -> JS 
-- writeAST map = fmap f $ toList map
--   where f k v = "let " <> k <> " = " <> v <> ";"    


-- -- | JSInteraction will be used as input to writing the final JS 
-- writeAST :: M.Map Name (Maybe JS) -> [JSInteraction]
-- writeAST map = fmap f $ toList map
--   where f k Nothing = "console.log(" <> k <> ");" -- best to run at the end and grab 
--         f k (Just a) = GetSet 




-- -- (SET, GET)
-- writeJSInteraction :: JSInteraction -> (JS, JS)
-- writeJSInteraction (Get name) = getJSVal name
-- writeJSInteraction (Set name valExpr) = setJSVal 
-- writeJSInteraction (SetGet ) = ""
-- writeJSInteraction (GetByExpr ) = ""

-- To grab all, requires Scraping and to Set any requires stream editing

-- streamEdit input <> get


-- streamEditGrab :: String -> (String, [Name])


-- streamEditGrab str >>= \(script, names) -> script <> (mconcat $ fmap getJSVal names)

-- :t f ast ~~~= function for streamEdit




-- parseOnly (convertToAtto json) (




-- (shit, thing) <- manyTill_ shitIDC thing
-- (<>) <$> (pure $ shit <> (f thing)) <*> continue 




-- I could also do one that grab





-- | ^^ SO: getAST :: Parser a -> Name -> a

-- Note too that, lets say we want to carry forward past vars

-- we could even write this AST at the beginning of the script

-- we also need a way to trim the map of jsVals





-- runVDOMWith html $ compileJS userMadeJS "someName"

-- Tuple a => a -> Args ~ String 


-- -- | I Could expand this to use a GADT that can carry forward simple values
-- -- | there is also a way that if I was to mutate the JSAST recursively
-- -- | that I could carry forward plain values
-- data JSAST = JSAST { script :: Text
--                    , ast :: Map Name (Maybe JS)
--                    }
             

-- data  JSAST = JSAST { varDecls :: [(Name, JSVal)]
--                       -- ^ overrides 
--                     , out :: [Name]
--                     , scriptyy :: Script
--                     , domUpdates :: Maybe [JS]
--                       -- ^ this is just to test an idea, would only apply to DOM environment case
--                     }

-- with setting variables + a script we may need to streamEdit the script
-- Actually for now we could just set lets to vars and this would be the most reliable
-- interop method



                   
-- updateAST :: JSAST -> JST IO JSAST 
--                   | SetWithJSExpr Name 

-- runVDOMST = updateAST . runVDOMWithAST 

-- JSAST -> JSInteraction


-- -- | You have to specify get of a variable for Haskell to be aware of it 
-- runJS :: JS -> MonadJS m ()
-- runJS js = do
--   let getAST = undefined
--   ast <- getAST 
--   runJSWithCliStatefully $ mkScript ast js 
--   where
--     mkScript = undefined




-- We could expand JSAST by adding "modules" which are just blocks of JS code which have their function
-- definitions 



-- mkFFI :: ExecutableName -> AST

-- I could also simplify my life by directly asking for the JS expression which may use
-- your predefined scripts and I can do this to make it Lazy where

--                                        I just parse the expression which has a definite format and
--                                        I can invalidate as well as having a clear reference system 

                               

-- The reason for JS Interaction is to compile our systems work with the users input
-- and both sources must follow the same format


-- type Url = Text -- String
-- getHtmlJS :: Url -> IO Html 
-- getHtmlJS url = undefined $ "JSDOM.fromURL(\"" <> url <>  "\", options).then(dom => {console.log(dom.serialize());});"
-- -- | TODO(galen): 
-- -- const cookieJar = new jsdom.CookieJar(store, options);
-- -- const dom = new JSDOM(``, { cookieJar });


-- in the above, if we knew, what they wanted to return, then we could allow a javascript expression


-- return = [ "a", "myVar" ]


-- return = [ ("a", Nothing)
--          , ("myVar", "1+1") -- on the left is a statement which can call any variables in the JS scope 
--          ]
         
-- >> console.log("a:" <jsConcat> a) -- this would return undefined if a doesnt exist
-- >> console.log("myVar:" <jsConcat> myVar)  -- we know this exists


      -- (fst ,) <$> (manyTill_ ((some $ noneOf ['\n']) <* (char '\n')) (string "!@#$%^&*()")) <*> many anyChar 
  
                   
-- scrape (string "|>" >> manyTill_ anyChar (string "|>")) string




-- looseSep sep p = do
--   optional sep
--   liftA2 (<>) p (looseSep sep p)  
  



  
  -- x <- some $ noneOf ['']
  -- optional (char '\n')
  -- xs <- maybeJSvals
  -- case xs of
  --   Just vals -> 
  -- pure (x:xs)
  
  

-- scrape (noneOf 
 
-- jsvals :: Stream s m Char => ParsecT s u m [JSVal]
-- jsvals = do
--   x <- some $ noneOf ['\n']
--   optional (char '\n')
--   xs <- maybeJSvals
--   case xs of
--     Just vals -> 
--   pure (x:xs)
  





-- -- | I could build on this by having a utility scraping function which tries a scraper
-- -- | and fallsback to runJSDOM .then(run scraper) in the case of a failure
-- runJSDOM :: Html -> IO Html
-- runJSDOM indexHtml = do
--   let scrMain =
--         "let htmlIn = `" <> (pack indexHtml) <> "`;"
--         <> "let dom = new jsDomLib.JSDOM(htmlIn, { runScripts: \"dangerously\" }) "
--       script = Script mempty $ "let jsDomLib = require('jsdom');" <> scrMain
--   fmap head $ runScript $ withScript script (JS "console.log(dom.serialize()
                                            
-- Scrape all script elems
-- Fetch if src attribute not null and set HTTP body to the key of the data struct
-- Append all script bodies together

-- 1. scrape (el "script" []) html



-- -- | I could build on this by having a utility scraping function which tries a scraper
-- -- | and fallsback to runJSDOM .then(run scraper) in the case of a failure
-- runJSDOMText :: Text -> IO Html
-- runJSDOMText indexHtml = do
--   let scrMain =
--         "let htmlIn = `" <> (indexHtml) <> "`;"
--         <> "let dom = new jsDomLib.JSDOM(htmlIn, { runScripts: \"dangerously\" }) "
--       script = ScriptText $ "let jsDomLib = require('jsdom');" <> scrMain
--   fmap head $ runScriptText $ withScriptText script (JS "console.log(dom.serialize())")
-- -- Scrape all script elems
-- -- Fetch if src attribute not null and set HTTP body to the key of the data struct
-- -- Append all script bodies together







-- newTest = runJSDOM htmlTest


-- -- TODO(galen): change return document to just being done in the JSM monad 
-- runScript :: Text -> IO Text
-- runScript script = do
--   ctxRef <- mkContextRef
--   runJSaddle ctxRef $ valToText =<< (eval ("1+1" :: Text) ) -- $ finallyGetDocument script)
--   where
--     finallyGetDocument :: Text -> Text
--     finallyGetDocument script = script <> "; return document;"


--- TAKEN FROM JSaddle run










  
