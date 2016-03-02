import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Text.HandsomeSoup
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Curl
import Control.Monad

lookk = do (p,x) <- curlGetString "http://www.imdb.com/" []
           case p of
               CurlOK -> return "anduvo"
               other -> return "NOO"

main = loop
    where loop = do m <- newEmptyMVar
                    forkIO $ putMVar m lookk
                    r <- takeMVar m
                    t <- r
                    print t
                    loop

{-
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r
-}
