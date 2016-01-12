import Common
import Find
import Notify

import Control.Concurrent
import Control.Concurrent.MVar

main :: IO ()
main = loop
    where 
    loop = do   
        n <- createNotify
--    m <- newEmptyMVar
        searchWord n
--    x <- takeMVar m
--    t <- x
--    print t
        loop







