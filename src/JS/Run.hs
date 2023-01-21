module JS.Run where

-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
fetchVDOMWith :: Link -> JS -> ExceptT ParseError IO (Html, [JSVal])
fetchVDOMWith (Link url) (JS js) = do
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript
  except $ parse jsStdOutParser "" rawJSVal 
  where
    mkVDOMScript = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
--      <> ("fs.readFile('" <> (pack fp) <> "', function(err, text) {")
      <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: false, userAgent: \"Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0\",});"
      <> "const virtualConsole = new jsdom.VirtualConsole();"
      <> "virtualConsole.sendTo(console, { omitJSDOMErrors: false });"
      <> "let options = { runScripts: \"dangerously\", resources: rLoader, virtualConsole};"
--      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader, virtualConsole});"
      <> "jsdom.JSDOM.fromURL(\"" <> (pack url) <> "\", options).then(document =>"
      <> js
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"
      <> ")"


-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
runVDOMWith :: BaseUrl -> Html -> JS -> ExceptT ParseError IO (Html, [JSVal])
runVDOMWith baseUrl indexHtml (JS js) = withTempFile "." "index.html" $ \htmlFPath handle -> do
  liftIO $ hPutStr handle $ pack indexHtml 
  liftIO $ hFlush handle
  liftIO $ print $ pack $ renderLink baseUrl
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath
  except $ parse jsStdOutParser "" rawJSVal 
  where
    -- mkVDOMScript fp = Script mempty
    --   $  "let fs = require('fs');"
    --   <> "let jsdom = require('jsdom');" 
    --   <> ("const text = fs.readFile(\"" <> (pack fp) <> "\").toString('utf-8');")
    --   <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: true, userAgent: \"Mellblomenator/9000\",});"
    --   <> "const virtualConsole = new jsdom.VirtualConsole();"
    --   <> "virtualConsole.sendTo(console, { omitJSDOMErrors: true });"
    --   <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
    --   <> ", url: \"" <> (pack baseUrl) <> "\", virtualConsole});"
    --   <> js
    --   <> (pack ['\n'])
    --   <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
    --   <> "console.log(dom.serialize());"

    mkVDOMScript fp = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
      <> ("fs.readFile('" <> (pack fp) <> "', function(err, text) {")
      <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: false, userAgent: \"Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0\",});"
      <> "const virtualConsole = new jsdom.VirtualConsole();"
      <> "virtualConsole.sendTo(console, { omitJSDOMErrors: false });"
      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
      <> ", url: \"" <> (pack . renderLink $ baseUrl) <> "\", virtualConsole});"
      <> js
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"
      <> "});"
