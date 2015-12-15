import Text.XML.HXT.Core
--import Network.URI
--import Text.HandsomeSoup
import Data.List
import Network.Curl
--import Text.XML.HXT.Curl
--import Text.XML.HXT.TagSoup
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time
import Data.Time.Clock.POSIX
	
get :: URLString -> IO String
get uri = do
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
    doc <- get url
    xml <- return $ parseXML doc
    return xml  

lookFunc :: URLString -> String -> IO (Int, [String])
lookFunc url w = do 
    xml <- getXML url
    result <- runX $ xml //> hasText (isInfixOf w) >>> getText
--    threadDelay $ round $ 60*1000000                 --seconds*1000000
    return (length result, result)                  

--    print $ length result
--    mapM_ putStrLn result 
                    

main = do 
    putStrLn "Ingrese URL"
    url <- getLine
    putStrLn "Ingrese palabra a buscar"
    w <- getLine   
    m <- newEmptyMVar
    forkIO $ putMVar m (lookFunc url w)
    t <- takeMVar m
    x <- t
    p <- getCurrentTime
--    p <- getPOSIXTime
    print p
    print x



