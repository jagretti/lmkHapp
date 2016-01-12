module Find where

import Common

import Control.Concurrent
import Text.XML.HXT.Core
import Network.Curl
import Data.List
--Prueba de poner tiempo!
import Data.Time


toSeconds :: Float -> Float
toSeconds x = x*3600

sleep :: Float -> IO ()
sleep n = threadDelay $ round $ (toSeconds n)*1000000

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

--search :: Notify -> IO Answer
searchWord p = do
    xml <- getXML $ url p
    result <- runX $ xml //> hasText (isInfixOf (request p)) >>> getText
    case result of
        [] -> do print $ "Intente "++ name p
                 sleep $ time p
                 searchWord p
        xs -> print xs
--        xs -> return (A (length result) result)  







