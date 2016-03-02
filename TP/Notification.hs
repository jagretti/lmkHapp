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

--hace todo el laburo
timePQ :: StateT Env IO ()
timePQ = do 
    ys <- get
    fstTime ys
    forever $ do
    xs <- get
    io $ print xs --borrar
    io $ check xs
    t <- getMinPQ
    diffAll t
    ys <- get
    ans <- io (waitAndLook t)
    case ans of
        Left error -> do tick t
                         handleError t error
        Right (A n []) -> do setDefault t
                             io $ print ("no encontre nada de "++n) --borrar
                             return ()
        Right (A n xs) -> do io $ okAnswer (fst' t) (A n xs)
                             io $ print ("borro por que ya encontre "++n) --borrar
                             deleteT t

handleError :: Tup -> CurlCode -> StateT Env IO ()
handleError t err = do
    let n = fst' t
    case err of
        CurlRecvError -> if thd' t > 5 then deleteT t >> return () else io (errorT n err) >> setDefault t >> return ()
        other -> do io $ errorT n other
                    deleteT t
                    return () 

tick :: Tup -> StateT Env IO ()
tick t = do modify (\s -> tick' t s)
            return ()
         where tick' t [] = []
               tick' t (x:xs) = if fst' t == fst' x then (fst' x,snd' x,thd' x +1):xs else x:(tick' t xs)

fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c

io :: IO a -> StateT Env IO a
io = liftIO

check :: Env -> IO ()
check xs = when (xs == []) exitSuccess

--el chequeo primero
fstTime :: Env -> StateT Env IO ()
fstTime [] = return ()
fstTime (x:xs) = do
    n <- io $ bigLookUp (fst' x)
    tick x
    case n of
        Left error -> do handleError x error
                         fstTime xs
        Right (A np []) -> do io $ print ("no recibi nada de "++np) --borrar
                              fstTime xs
        Right (A np ys) -> do io $ okAnswer (fst' x) (A np ys)
                              io $ print ("borro "++np++" la encontre") --borrar
                              deleteT x
                              fstTime xs

waitAndLook :: Tup -> IO (Either CurlCode Answer)    
waitAndLook (n,t,e) = do 
    threadDelay $ t * 60000000
    r <- bigLookUp n
    case r of
        Right a -> return $ Right a
        Left err -> return $ Left err
        
diffAll :: Tup -> StateT Env IO ()
diffAll (n,t,e) = do
    modify (\s -> map (\(n1,t1,e) -> if n == n1 then (n1,t1,e) else (n1,t1-t,e)) s) 
    return ()     

getMinPQ :: StateT Env IO Tup
getMinPQ = do
    xs <- get
    return $ findMin xs

findMin :: [Tup] -> Tup
findMin [x] = x
findMin (x:y:xs) = if snd' x <= snd' y then findMin (x:xs) else findMin (y:xs)

deleteT :: Tup -> StateT Env IO ()
deleteT t = do modify (\s -> del t s)
               return ()
    where del t [] = []
          del t (x:xs) = if fst' t == fst' x then xs else x:(del t xs)

setDefault :: Tup -> StateT Env IO ()
setDefault t = do 
    modify (\s -> deflt t s)
    return ()
        where deflt t [] = []
              deflt t ((n,ti,e):xs) = if fst' t == n then (n,time n,e):xs else (n,ti,e):(deflt t xs)  

        
