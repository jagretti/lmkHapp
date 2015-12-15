module Find where

import Common

import Text.XML.HXT.Core
import Network.Curl
import Data.List


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
    xml <- return $ parseXML doc
    return xml

search :: Notify -> IO Answer
search p = do
    xml <- getXML $ url p
    result <- runX $ xml //> hasText (isInfixOf (request p)) >>> getText
    return (A (length result) result)                








