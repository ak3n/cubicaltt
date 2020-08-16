{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson as JSON
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Exception
import Control.Lens ((^.))
import Data.List as L
import Data.Functor
import Data.Time
import Data.Traversable
import Data.Text as T
import Data.ByteString.Lazy as BS
import Data.Text.Encoding as T
import System.Directory
import System.FilePath
import System.Environment
import System.Console.GetOpt
import Text.Printf

import Exp.Lex
import Exp.Par
import Exp.Print
import Exp.Abs hiding (NoArg)
import Exp.Layout
import Exp.ErrM
import Prelude as P

import CTT hiding (def)
import Resolver
import qualified TypeChecker as TC
import qualified Eval as E

import Data.Aeson
import Data.Maybe
import Data.FileEmbed
import Reflex.Dom
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Element (getElementsByTagName, setAttribute)
import GHCJS.DOM.HTMLCollection (itemUnsafe)
import GHCJS.DOM.Types as DOM hiding (Text, Event)
import GHCJS.DOM.NonElementParentNode (getElementById)
import qualified Language.Javascript.JSaddle as JS

import Helpers

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget :: Widget t ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("http-equiv" =: "x-ua-compatible" <> "content" =: "ie-edge") blank
  el "title" (text "CubicalTT")
  elAttr "meta" ("name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1") blank
  elAttr "link" ("rel" =: "stylesheet"
    <> "href" =: "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
    <> "integrity" =: "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
    <> "crossorigin" =: "anonymous") blank
  elAttr "link" ("data-name" =: "vs/editor/editor.main"
    <> "rel" =: "stylesheet"
    <> "href" =: "https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs/editor/editor.main.css") blank
  el "style" $ text "html, body { height: 100%; }"

bodyWidget :: Widget t ()
bodyWidget = do
  elAttr "div" ("class" =: "container-fluid d-flex flex-column" <> "style" =: "min-height: 100%;" ) $ do
    elClass "div" "row" $ do
      elClass "div" "col-7 mt-2" $ do
        elClass "ul" "nav nav-tabs" $ do
          elClass "li" "nav-item" $
            elAttr "a" ("id" =: "welcome-tab"
              <> "href" =: "#" <> "class" =: "nav-link tab-link active") $ text "welcome"
          elClass "li" "nav-item" $
            elAttr "a" ("id" =: "lecture1-tab"
              <> "href" =: "#" <> "class" =: "nav-link tab-link") $ text "lecture1"
          elClass "li" "nav-item" $
            elAttr "a" ("id" =: "lecture2-tab"
              <>"href" =: "#" <> "class" =: "nav-link tab-link") $ text "lecture2"
          elClass "li" "nav-item" $
            elAttr "a" ("id" =: "lecture3-tab"
              <> "href" =: "#" <> "class" =: "nav-link tab-link") $ text "lecture3"
          elClass "li" "nav-item" $
            elAttr "a" ("id" =: "lecture4-tab"
              <> "href" =: "#" <> "class" =: "nav-link tab-link") $ text "lecture4"
    elClass "div" "row flex-grow-1" $ do
      elClass "div" "col-7 flex-grow-1" $ do
        elAttr "div" ("id" =: "editor" <> "class" =: "flex-grow-1"
          <> "style" =: "width:auto;height:600px;border:1px solid grey") blank
      elClass "div" "col-5 pl-0 flex-grow-1" $ do
        elAttr "div" ("class" =: "col-md flex-grow-1" <> "id" =: "results-container"
          <> "style" =: "width:auto;height:600px;border:1px solid grey;overflow-x:scroll;") $
            el "pre" $ elAttr "code" ("id" =: "results") blank
        elClass "div" "col-*-* w-100" $ do
          elClass "div" "input-group mb-3" $ do
            elClass "div" "input-group-prepend" $
              elClass "span" "input-group-text" $ text ">"
            elAttr "input" ("id" =: "eval-input"
              <> "type" =: "text"
              <> "class" =: "form-control"
              <> "style" =: "box-shadow: none;") blank
            elClass "div" "input-group-append" $
              elAttr "button" ("id" =: "eval-button" <> "type" =: "button"
                <> "class" =: "btn btn-outline-secondary") $ text "Eval"
  let
    vsPath = "https://microsoft.github.io/monaco-editor/node_modules/monaco-editor/min/vs/"
  el "script" $ text $ "var app = {}; var require = { paths: { 'vs': '" <> vsPath <>  "'} };"
  elAttr "script" ("src" =: (vsPath <> "loader.js")) blank
  elAttr "script" ("src" =: (vsPath <> "editor/editor.main.nls.js")) blank
  elAttr "script" ("src" =: (vsPath <> "editor/editor.main.js")) blank
  DOM.liftJSM $ do
    setupEditorJS <- JS.eval editorSetupScript
    void $ JS.call setupEditorJS setupEditorJS
      [ toJSVal welcomeContent
      , toJSVal lecture1Content
      , toJSVal lecture2Content
      , toJSVal lecture3Content
      , toJSVal lecture4Content
      , toJSVal (JS.fun loadHandler), toJSVal (JS.fun evalHandler)]

evalHandler :: JSVal -> JSVal -> [JSVal] -> JSM ()
evalHandler _ _ [tabName, inputContent] = do
  currentTabName <- JS.valToText tabName
  mCache <- loadTabCache "repl"
  case mCache of
    Just (names, tenv) -> do
      val <- JS.valToText inputContent
      evalInput names tenv val
    _ -> do
      writeLogAndScroll $ "Couldn't load the cache for REPL. Please, try to load the buffer\n"

allTabsNames :: [Text]
allTabsNames =
  [ "welcome"
  , "lecture1"
  , "lecture2"
  , "lecture3"
  , "lecture4"]

loadTabContent :: Text -> JSM TabWithContent
loadTabContent name = do
  loadTabContentJS <- JS.eval $ T.unlines
    [ "(function(name) {"
    , "  return app.tabs[name + '-tab'][0].getValue();"
    , "})"
    ]
  tabContentVal <- JS.call loadTabContentJS loadTabContentJS [toJSVal name]
  val <- JS.valToText tabContentVal
  return (name, val)

loadAllTabs :: JSM [TabWithContent]
loadAllTabs = do
  tabsContentVals <- traverse loadTabContent allTabsNames
  tabsContent <- traverse JS.valToText tabsContentVals
  return $ P.zip allTabsNames tabsContent

loadTabCache :: Text -> JSM (Maybe CheckedModule)
loadTabCache tabName = do
  mNames :: Maybe Names <-
    loadDataFromLocalStorage ("load-results-names-" <> tabName)
  mImports :: Maybe TC.TEnv <-
    loadDataFromLocalStorage ("load-results-imports-" <> tabName)
  case (mNames, mImports) of
    (Just names, Just imports) -> pure $ Just (names, imports)
    _ -> pure Nothing

loadHandler :: JSVal -> JSVal -> [JSVal] -> JSM ()
loadHandler _ _ [tabNameVal, contentVal] = do
  currentTabName <- JS.valToText tabNameVal
  currentInput <- JS.valToText contentVal
  (_, _, loadedMods) <- processImports ([],[],[]) (currentTabName, currentInput)
  initLoop loadedMods

getModule :: TabWithContent -> JSM (Maybe Module)
getModule (tabName, input) = do
  -- mModule <- loadDataFromLocalStorage $ "cache-module-" <> tabName
  -- case mModule of
  --   Just m -> return $ Just m
  --   Nothing -> do
  let
    ts = lexer $ T.unpack input
  case pModule ts of
    Bad s -> do
      writeLogAndScroll $ T.pack $
        "Parse failed in " ++ show tabName ++ "\n" ++ show s ++ "\n"
      return Nothing
    Ok mod -> do
      -- saveDataToLocalStorage ("cache-module-" <> tabName) mod
      return $ Just mod

processImports
  :: ([Text],[String],[Module])
  -> TabWithContent
  -> JSM ([Text],[String],[Module])
processImports st@(notok,loaded,mods) twc@(tabName, input) = do
  mMod <- getModule twc
  case mMod of
    Nothing -> return st
    Just mod@(Module (AIdent (_,name)) imp decls) -> do
      let
        importModsNames = [T.pack i | Import (AIdent (_,i)) <- imp]
      importMods <- traverse loadTabContent $
        P.filter (\i -> i `P.elem` allTabsNames) importModsNames
      if (name /= T.unpack tabName) then do
        writeLogAndScroll $ T.pack $ "Module name mismatch in "
          ++ show tabName ++ " with wrong name " ++ name
        return st
      else do
        (notok1,loaded1,mods1) <- foldM processImports (tabName:notok,loaded,mods) importMods
        return (notok,loaded1,mods1 ++ [mod])

-- cubicaltt functions
type Names = [(CTT.Ident,SymKind)]

type TabWithContent = (Text, Text)

type CheckedModule = (Names, TC.TEnv)

initLoop
  :: [Module]
  -> JSM ()
initLoop [] = pure ()
initLoop mods = do
  case runResolver $ resolveModules mods of
    Left err ->
      writeLogAndScroll $ T.pack $ "Resolver failed: " ++ err ++ "\n"
    Right (adefs,names) -> do
      let ns = fmap fst names
          dups = ns \\ nub ns
      unless (dups == []) $
        writeLogAndScroll $ T.pack $ "Warning: the following definitions were shadowed [" ++
          L.intercalate ", " dups ++ "]\n"
      saveDataToLocalStorage "load-results-names-repl" names
      saveDataToLocalStorage "decls-to-process" adefs
      TC.runDeclss' TC.verboseEnv
              
evalInput :: Names -> TC.TEnv -> Text -> JSM ()
evalInput names tenv input = do
  writeLogAndScroll $ "> " <> input <> "\n"
  let
    (msg, str, mod) = if T.isInfixOf ":n" input
      then ("NORMEVAL: ", T.drop 2 input, E.normal [])
      else ("EVAL: ", input, id)
  case pExp (lexer $ T.unpack str) of
    Bad err -> writeLogAndScroll $ T.pack $ ("Parse error: " ++ err) <> "\n"
    Ok exp -> case runResolver $ local (insertIdents names) $ resolveExp exp of
      Left err -> writeLogAndScroll $ T.pack $ ("Resolver failed: " ++ err) <> "\n"
      Right body -> do
        x <- TC.runInfer tenv body
        case x of
          Left err -> writeLogAndScroll $ T.pack $ ("Could not type-check: " ++ err) <> "\n"
          Right _ -> do
            let
              e = mod $ E.eval (TC.env tenv) body
            writeLogAndScroll $ msg <> (T.pack $ show e) <> "\n"

lexer :: String -> [Token]
lexer = resolveLayout True . myLexer

-- files

welcomeContent :: Text
welcomeContent = decodeUtf8 $ $(embedFile "welcome.ctt")

lecture1Content :: Text
lecture1Content = decodeUtf8 $ $(embedFile "lectures/lecture1.ctt")

lecture2Content :: Text
lecture2Content = decodeUtf8 $ $(embedFile "lectures/lecture2.ctt")

lecture3Content :: Text
lecture3Content = decodeUtf8 $ $(embedFile "lectures/lecture3.ctt")

lecture4Content :: Text
lecture4Content = decodeUtf8 $ $(embedFile "lectures/lecture4.ctt")

editorSetupScript :: Text
editorSetupScript = decodeUtf8 $ $(embedFile "setup.js")