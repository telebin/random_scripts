{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Either (fromRight)
import Data.Maybe (fromJust, isJust)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Snap.Core
import Snap.Http.Server (quickHttpServe)

import qualified Data.ByteString.Char8 as BSC
import qualified Text.JSON as Json

type Translator = Json.Result Json.JSValue -> [String]

parseDiki :: Translator
parseDiki (Json.Error x) = [x]
parseDiki (Json.Ok (Json.JSArray strs)) = map conv strs
  where
    conv (Json.JSString str) = Json.fromJSString str
    conv _ = "Just not expected"
parseDiki _ = ["Invalid JSON format"]

parseSjp :: Translator
parseSjp (Json.Error x) = [x]
parseSjp (Json.Ok (Json.JSArray objs)) = map (conv . unpack) objs
  where
    unpack (Json.JSObject obj) = let [_, value, _] = Json.fromJSObject obj in snd value
    conv (Json.JSString str) = Json.fromJSString str
    conv _ = "Just not expected"
parseSjp _ = ["Invalid JSON format"]

prepareAnswer :: String -> [String] -> String
prepareAnswer q ans = Json.encode (q, ans)

get :: Manager -> String -> IO BSC.ByteString
get manager address = do
  request <- parseRequest address
  response <- httpLbs request manager
  return $ toStrict $ responseBody response

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    quickHttpServe $ suggestionTranslatorRouter manager

suggestionTranslatorRouter :: Manager -> Snap ()
suggestionTranslatorRouter manager =
    route [ ("di/:searchString", dikiHandler manager)
          , ("sjp/:searchString", sjpHandler manager)
          ]

dikiHandler :: Manager -> Snap ()
dikiHandler = translateRequest "https://www.diki.pl/dictionary/autocomplete?langpair=en%3A%3Apl&q=" parseDiki

sjpHandler :: Manager -> Snap ()
sjpHandler = translateRequest "https://sjp.pwn.pl/complete.php?source=autocomplete-sjp&query=" parseSjp

translateRequest :: String -> Translator -> Manager -> Snap ()
translateRequest address translate manager = do
    param <- getParam "searchString"
    if isJust param then do
        let toSearch = fromJust param
        logError $ BSC.concat ["Searching: ", toSearch, " with query: ", BSC.pack address]
        res <- liftIO $ get manager (address ++ BSC.unpack toSearch)
        logError $ BSC.append "Got result:\n" res
        writeBS $ utf8ByteString $ prepareAnswer (BSC.unpack toSearch) (translate $ Json.decode $ BSC.unpack res)
    else writeBS "searchString parameter missing"
  where
    utf8ByteString = toStrict . toLazyByteString . stringUtf8
