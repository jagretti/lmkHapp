import Text.XML.HXT.Core
--import Network.URI
import Text.HandsomeSoup
import Data.List
import Network.Curl
--import Text.XML.HXT.Curl
--import Text.XML.HXT.TagSoup
--import Control.Concurrent
--import Control.Concurrent.Async
--import Control.Concurrent.MVar
--import Data.Time
--import Data.Time.Clock.POSIX
import System.Console.Readline
import Data.Maybe
	

url3 = "http://www.fceia.unr.edu.ar/lcc/r313/"
url = "http://www.fceia.unr.edu.ar/lcc/r111/"
url1 = "https://ar.tiempo.yahoo.com/"
url2 = "http://www.smn.gov.ar/?mod=pron&id=4&provincia=Santa%20Fe&ciudad=Rosario"

get :: IO String
get = do
    eresp <- curlGetString url []
    case fst eresp of
        CurlOK -> return $ snd eresp
        _ -> do print $ fst eresp
                return $ snd eresp

parseXML :: String -> IOStateArrow s b XmlTree
parseXML st = do 
    readString [ withParseHTML      yes
               , withWarnings       no
               ] st

getXML :: IO (IOStateArrow s b XmlTree)
getXML = do 
    doc <- get
    xml <- return $ parseXML doc
    return xml  

lookFunc :: URLString -> String -> IO ()
lookFunc url w = do 
    xml <- getXML
    result <- runX $ xml //> hasText (isInfixOf w) >>> getText
        
--    threadDelay $ round $ 20*1000000                 --seconds*1000000
--    return (length result, result)                  

    print $ length result
    mapM_ putStrLn result 

readK :: String -> IO String
readK s = do
    t <- readline s
    case t of
        Nothing -> do putStrLn "Intente nuevamente"
                      readK s
        Just a -> case a of
                      [] -> do putStrLn "Intente nuevamente"
                               readK s
                      _ -> return a 
                    
main = do
    html <- getXML
--    links <- runX $ html //> css "a" //> getText
--    pes <- runX $ html //> css "p" //> getText
--    word <- runX $ html //> hasText (isInfixOf "Mendoza") >>> getText
--    clas <- runX $ html //> ifA (getAttrValue "class" >>> (hasText "current")) (getChildren >>> getText) (this) 
    consulta <- runX $ html //> hasText (isInfixOf "Consultas") >>> getText  
    print consulta
















