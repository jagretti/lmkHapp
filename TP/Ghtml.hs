module Ghtml where

import Common

import Control.Concurrent
import Text.XML.HXT.Core
import Network.Curl
import Data.List
--Prueba de poner tiempo!
--import System.Clock

sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1000000

minToSec :: Int -> Int
minToSec n = case n of
             30 -> 1800
             60 -> 3600
             120 -> 7200

--CurlUserAgent "Mozilla/5.0 (en-US) Firefox/2.0.0.6667"

getPage :: URLString -> IO String
getPage uri = do
    eresp <- curlGetString uri []
    case fst eresp of
        CurlOK -> return $ snd eresp
        _ -> do print $ fst eresp
                return $ snd eresp

parseXML :: String -> IOStateArrow s b XmlTree
parseXML st = do 
    readString [ withParseHTML      yes
               , withWarnings       no
               ] st

getXML :: URLString -> IO (IOStateArrow s b XmlTree)
getXML url = do 
    doc <- getPage url
    return $ parseXML doc
    








