module Notification where

import Common
import Ghtml
import Lookups
import TypeN

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.ST
import Data.List
import Control.Monad
import Control.Monad.Trans.State.Lazy
import System.Exit
import Network.Curl

--Loop principal con una Lista de prioridad. La prioridad la tiene la Tup con el menor tiempo cambiante
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

--Maneja que hacer si hay un error
handleError :: Tup -> CurlCode -> StateT Env IO ()
handleError t err = do
    let n = fst' t
    case err of
        CurlRecvError -> if thd' t > 4 then deleteT t >> return () else io (errorT n err) >> setT t 0 >> return () --OJO que cambie defaultT por setT!!!!!!!
        other -> do io $ errorT n other
                    deleteT t
                    return () 
                                
--Suma 1 a la cantidad de errores de la Tup dada
tick :: Tup -> StateT Env IO ()
tick t = do modify (\s -> tick' t s)
            return ()
         where tick' t [] = []
               tick' t (x:xs) = if fst' t == fst' x then (fst' x,snd' x,thd' x +1):xs else x:(tick' t xs)

--Funcionas de Tupla de 3
fst' (a,_,_) = a
snd' (_,b,_) = b
thd' (_,_,c) = c

--Solo comodidad
io :: IO a -> StateT Env IO a
io = liftIO

--Para salir con exito del forever
check :: Env -> IO ()
check xs = when (xs == []) exitSuccess

--El chequeo inicial
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

--Espera y busca
waitAndLook :: Tup -> IO (Either CurlCode Answer)    
waitAndLook (n,t,e) = do 
    threadDelay $ t * 60000000
    r <- bigLookUp n
    case r of
        Right a -> return $ Right a
        Left err -> return $ Left err

--Resta a todas las Tup, el tiempo cambiante de la Tup dada        
diffAll :: Tup -> StateT Env IO ()
diffAll t = do
    modify (\s -> diffA t s) 
    return ()
        where diffA t [] = []
              diffA t ((n',t',e'):xs) = if fst' t == n' then (n',t',e'):(diffA t xs) else (n',t' - snd' t,e'):(diffA t xs)     

--Consigue la Tup con el menor tiempo cambiante
getMinPQ :: StateT Env IO Tup
getMinPQ = do
    xs <- get
    return $ findMin xs
        where findMin [x] = x
              findMin (x:y:xs) = if snd' x <= snd' y then findMin (x:xs) else findMin (y:xs)

--Borra la Tup dada del State
deleteT :: Tup -> StateT Env IO ()
deleteT t = do 
    modify (\s -> del t s)
    return ()
        where del t [] = []
              del t (x:xs) = if fst' t == fst' x then xs else x:(del t xs)

--Setea a default (creo que no hace falta ahora que esta setT)
setDefault :: Tup -> StateT Env IO ()
setDefault t = do 
    modify (\s -> deflt t s)
    return ()
        where deflt t [] = []
              deflt t ((n,ti,e):xs) = if fst' t == n then (n,time n,e):xs else (n,ti,e):(deflt t xs)  

--Setea el tiempo de la Tup al Int dado
setT :: Tup -> Int -> StateT Env IO ()
setT t n = do 
    modify (\s -> sett t n s)
    return ()
        where sett t n [] = []
              sett t n ((n',ti,e):xs) = if fst' t == n' then (n',n,e):xs else (n',ti,e):(sett t n xs)  



        
