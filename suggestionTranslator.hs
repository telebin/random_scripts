{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Either (fromRight)
import Network.HTTP.Client
import Snap.Core
import Snap.Http.Server (quickHttpServe)

import qualified Text.JSON as Json

parseDiki :: Json.Result Json.JSValue -> Either String [String]
parseDiki (Json.Error x) = Left x
parseDiki (Json.Ok (Json.JSArray strs)) = Right $ map conv strs where
    conv (Json.JSString str) = Json.fromJSString str
    conv _ = "Just not expected"
parseDiki _ = Left "Invalid JSON format"

prepareAnswer :: String -> [String] -> String
prepareAnswer q ans = Json.encode (q, ans)

dikiAnsw = "[\"hen\",\"Henryk\",\"Henry\",\"Henryka\",\"hence\",\"Henrietta\",\"henceforth\",\"henchman\",\"henna\",\"henhouse\",\"Henoch\",\"Henize 3-1357\",\"hendiadys\",\"henogamia\",\"henoteistyczny\",\"henoteizm\",\"hentai\",\"heneken\",\"Henle's layer\",\"Henle's loop\",\"Henriad\",\"Henrician Articles\",\"Henry rifle\",\"Hensen's cell\",\"H\\u00e9non map\",\"hen harrier\",\"hen run\",\"henbane\",\"henbit\",\"henbit dead-nettle\",\"hench\",\"henchperson\",\"henchwoman\",\"hendecagon\",\"hendecagonal\"]"
sjpAnsw = "[{\"label\":\"<b>pen<\\/b>akordancja\",\"value\":\"penakordancja\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>alista\",\"value\":\"penalista\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>alistka\",\"value\":\"penalistka\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>alizacja\",\"value\":\"penalizacja\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>alny\",\"value\":\"penalny\",\"language\":[\"sjp\"]},{\"label\":\"<b>Pen<\\/b>ang\",\"value\":\"Penang\",\"language\":[\"sjp\"]},{\"label\":\"<b>Pen<\\/b>club\",\"value\":\"Penclub\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>del\",\"value\":\"pendel\",\"language\":[\"sjp\"]},{\"label\":\"<b>Pen<\\/b>derecki\",\"value\":\"Penderecki\",\"language\":[\"sjp\"]},{\"label\":\"<b>pen<\\/b>dlowa\\u0107\",\"value\":\"pendlowa\\u0107\",\"language\":[\"sjp\"]}]"
fdAnsw = "SAYT.Callback([\"mend\", [[\"mend\",3125],[\"mend (one's) fences\",1024],[\"mend (one's) pace\",1024],[\"mend (one's) ways\",1024],[\"mend bones\",2061],[\"mend fences\",1024],[\"mend her fences\",1024],[\"mend her pace\",1024],[\"mend her ways\",1024],[\"mend his fences\",1024]]])"

get manager address = do
--   manager <- newManager defaultManagerSettings
  request <- parseRequest address
  response <- httpLbs request manager
  responseBody response

main :: IO ()
main = quickHttpServe translateSuggestion

translateSuggestion :: Snap ()
translateSuggestion =
    route [ ("di/:searchString", dikiHandler)
          , ("sjp/:searchString", sjpHandler)
          , ("fd/:searchString", fdHandler)
          ]

dikiHandler :: Snap ()
dikiHandler = do
    param <- getParam "searchString"
    address = "https://www.diki.pl/dictionary/autocomplete?langpair=en%3A%3Apl&q=" ++ param
    writeBS param

sjpHandler :: Snap ()
sjpHandler = do
    param <- getParam "searchString"
    writeBS param

fdHandler :: Snap ()
fdHandler = do
    param <- getParam "searchString"
    writeBS param
