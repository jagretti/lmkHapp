module TypeN where

import Common
import Ghtml

import System.IO
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Network.Curl

inFile :: Notification -> Answer -> IO ()
inFile n a = do
    let nn = name n
    let nfile = nn++".log"
    t <- getCurrentTime
    appendFile nfile (nn++" "++show(statements a)++" "++show t++"\n")

printIt :: Notification -> Answer -> IO ()
printIt n a = do
    let nn = name n
    t <- getCurrentTime
    putStrLn $ nn++" "++show(statements a)++" "++show t

okAnswer :: Notification -> Answer -> IO ()
okAnswer n a = do
    case (ntype n) of   
        Log -> inFile n a
        Print -> printIt n a

errorT :: Notification -> CurlCode -> IO ()
errorT n err = do
    let nn = name n
    let nfile = nn++".log"
    t <- getCurrentTime
    case (ntype n) of
        Log -> appendFile nfile (nn++" "++curlError err++show t++"\n")
        Print -> putStrLn $ nn++" "++curlError err++" "++show t
        
