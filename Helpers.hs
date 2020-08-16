{-# LANGUAGE BangPatterns #-}
module Helpers where

import Control.Monad
import Data.Aeson as JSON
import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString.Lazy as BS
import GHCJS.DOM.Types as DOM hiding (Text, Event)
import qualified Language.Javascript.JSaddle as JS


writeLogAndScroll :: Text -> JSM ()
writeLogAndScroll !log = do
  -- add results
  !addResult <- JS.eval $ T.unlines
    [ "(function(f) {"
    , " app.log_buffer.push(f);"
    , "})" ]
  void $! JS.call addResult addResult [ toJSVal log ]

consoleLog :: Text -> JSM ()
consoleLog t = do
  log <- JS.eval $ T.unlines [
    "(function(msg) {",
    "  console.log(msg);",
    "})"]
  void $ JS.call log log
    [ toJSVal t
    ]

setTimeout :: JSVal -> Int -> JSM ()
setTimeout val t = do
  setTimeoutJS <- JS.eval $ T.unlines [
    "(function(f, t) {",
    "  setTimeout(f, t);",
    "})"]
  m <- toJSVal t
  void $ JS.call setTimeoutJS setTimeoutJS [ val, m ]

saveDataToLocalStorage :: ToJSON a => Text -> a -> JSM ()
saveDataToLocalStorage key value = do
  save <- JS.eval $ T.unlines [
    "(function(key, value) {",
    "  localStorage.setItem(key, value);",
    "})"]
  void $ JS.call save save
    [ toJSVal key
    , toJSVal . T.decodeUtf8 . BS.toStrict $ encode value
    ]

removeLocalStorage :: Text -> JSM ()
removeLocalStorage key = do
  save <- JS.eval $ T.unlines [
    "(function(key, value) {",
    "  localStorage.removeItem(key);",
    "})"]
  void $ JS.call save save
    [ toJSVal key
    ]

loadDataFromLocalStorage :: FromJSON a => Text -> JSM (Maybe a)
loadDataFromLocalStorage key = do
  getItem <- JS.eval $ T.unlines [
    "(function(key) {",
    "  return localStorage.getItem(key);",
    "})"]
  res <- JS.call getItem getItem [toJSVal key]
  (JSON.decode . BS.fromStrict . T.encodeUtf8) <$> JS.valToText res
