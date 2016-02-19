import System.Cron.Schedule
import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import System.Cron
import System.Console.Readline

{-
main :: IO ()
main = forever $ do
         now <- getCurrentTime
         when (scheduleMatches schedule now) doWork
         putStrLn "sleeping"
         threadDelay (10*100000)
         t <- getLine
         print t
       where doWork   = putStrLn "Time to work"
             schedule = everyMinute
-}


main = forever $ do
    tids <- execSchedule $ do
            addJob job1 "* * * * *"
            addJob job2 "0 * * * *"
    print tids


job1 = putStrLn "job1"

job2 = putStrLn "job2"

