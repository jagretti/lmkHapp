import Common
import Find
import Notify

import Control.Concurrent
import Control.Concurrent.MVar

main :: IO ()
main = do 
    n <- createNotify
    m <- newEmptyMVar
    forkIO $ putMVar m (search n)
    x <- takeMVar m
    t <- x    
    print t

prompt = ">>>"



