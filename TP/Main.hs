import Common
import Find
import Notify

import Control.Concurrent
import Control.Concurrent.MVar

{-
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
-}

main :: IO ()
main = loop
    where 
    loop = do   
        n <- createNotify
--    m <- newEmptyMVar
        forkIO $ search n
--    x <- takeMVar m
--    t <- x
--    print t
        loop

prompt = ">>>"

{-
main :: IO ()
main = do 
    l <- new    
    loop l
    where 
        loop l = do
            n <- createNotify
            forkIO $ search n l
            
            loop l
-}





