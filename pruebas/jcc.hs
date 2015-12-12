import Text.XML.HXT.Core
import Network.URI
import Text.HandsomeSoup
import Data.List
import Network.Curl
import Text.XML.HXT.Curl
import Text.XML.HXT.TagSoup
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe

get uri = do
  eresp <- curlGetString uri []
  return $ snd eresp

parseXML st = do readString [ withParseHTML      yes
                            , withWarnings       no
                            ] st


getXML url = do doc <- get url
                xml <- return $ parseXML doc
                return xml  

lookFunc url w = do xml <- getXML url
                    result <- runX $ xml //> hasText (isInfixOf w) >>> getText
                    threadDelay $ round $ 60*1000000
                    return (length result, result)                  

--                    print $ length result
--                    mapM_ putStrLn result
                    

main = do putStrLn "Ingrese URL"
          url <- getLine
          putStrLn "Ingrese palabra a buscar"
          w <- getLine   
          m <- newEmptyMVar
          forkIO $ putMVar m (lookFunc url w)
          t <- takeMVar m
          print (unsafePerformIO t)
          main


