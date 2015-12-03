import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Text.HandsomeSoup
import Data.List
import Data.Algorithm.Diff
import Control.Concurrent
import Control.Concurrent.MVar

main = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r
