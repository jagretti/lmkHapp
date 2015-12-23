module Find where

import Common

import Control.Concurrent
import Text.XML.HXT.Core
import Network.Curl
import Data.List
--Prueba de poner tiempo!
import Data.Time

import Data.Map (Map)
import qualified Data.Map as Map

type List = Map Notify Answer
newtype ListState = ListState (MVar List)

new :: IO ListState
new = do
    m <- newMVar Map.empty
    return (ListState m)

addTo :: Notify -> Answer -> ListState -> IO ()
addTo n a (ListState m) = do
    list <- takeMVar m
    putMVar m (Map.insert n a list)

lookup :: ListState -> Notify -> IO (Maybe Answer)
lookup (ListState m) n = do
    list <- takeMVar m
    putMVar m list
    return (Map.lookup n list)

printList :: ListState -> IO ()
printList (ListState m) = do x <- takeMVar m
                             print x


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
{-
tickN :: Notify -> IO Notify
tickN (N m a b c n) = return (N m a b c (n+1))
-}
--search :: Notify -> IO Answer
search p = do
    xml <- getXML $ url p
    result <- runX $ xml //> hasText (isInfixOf (request p)) >>> getText
--    print result
--    t <- getCurrentTime
--    print t
    case result of
        [] -> do print $ "Intente "++ name p
                 sleep $ time p
                 search p
        xs -> print xs
--        xs -> return (A (length result) result)  







