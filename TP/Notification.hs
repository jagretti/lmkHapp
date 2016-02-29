module Notification where

import Common
import Ghtml
import Lookups
import TypeN

import Control.Concurrent
import Control.Monad.State
import Data.List
import Control.Monad
import Control.Monad.Trans
import System.Exit
import Network.Curl
import Network.Curl.Debug

type Error = String
type Tup = (Notification,Int)
type Env = [Tup]


timePQ :: StateT Env IO ()
timePQ = do 
    ys <- get
    fstTime ys
    forever $ do
    xs <- get
    io $ print xs
    io $ check xs
    t <- getMinPQ
    diffAll t
    ys <- get
    ans <- io (waitAndLook t)
    case ans of
        Left error -> handleError t error
        Right (A n []) -> do setDefault t
                             io $ print ("no encontre nada de "++n)
                             return ()
        Right (A n xs) -> do io $ okAnswer (fst t) (A n xs)
                             io $ print ("borro por que ya encontre "++n)
                             deleteT t

handleError :: Tup -> CurlCode -> StateT Env IO ()
handleError t err = do
    let n = fst t
    case err of
        CurlRecvError -> do io $ errorT n err --DEBERIA DEFAULTEAR EL TIME....VER!!! O SINO PROBAR UNA VEZ MAS Y BORRARLA.
                            return ()
        other -> do io $ errorT n other
                    deleteT t
                    return () 

io :: IO a -> StateT Env IO a
io = liftIO

check :: Env -> IO ()
check xs = when (xs == []) exitSuccess

fstTime :: Env -> StateT Env IO ()
fstTime [] = return ()
fstTime (x:xs) = do
    n <- io $ bigLookUp (fst x)
    case n of
        Left error -> do handleError x error
                         fstTime xs
        Right (A np []) -> do io $ print ("no recibi nada de "++np)
                              fstTime xs
        Right (A np ys) -> do io $ okAnswer (fst x) (A np ys)
                              io $ print ("borro "++np++" la encontre")
                              deleteT x
                              fstTime xs

waitAndLook :: Tup -> IO (Either CurlCode Answer)    
waitAndLook (n,t) = do 
    threadDelay $ t * 60000000
    debug "hollllla"
    r <- bigLookUp n
    case r of
        Right a -> return $ Right a
        Left err -> return $ Left err
        
diffAll :: Tup -> StateT Env IO ()
diffAll (n,t) = do
    modify (\s -> map (\(n1,t1) -> if n == n1 then (n1,t1) else (n1,t1-t)) s) 
    return ()     

getMinPQ :: StateT Env IO Tup
getMinPQ = do
    xs <- get
    return $ findMin xs

findMin :: [Tup] -> Tup
findMin [x] = x
findMin (x:y:xs) = if snd x <= snd y then findMin (x:xs) else findMin (y:xs)

deleteT :: Tup -> StateT Env IO ()
deleteT t = do modify (\s -> del t s)
               return ()
    where del t [] = []
          del t (x:xs) = if t == x then xs else x:(del t xs)

setDefault :: Tup -> StateT Env IO ()
setDefault t = do 
    modify (\s -> deflt t s)
    return ()
        where deflt t [] = []
              deflt t (x:xs) = if t == x then (fst t,time (fst t)):xs else x:(deflt t xs)  

        
