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

--Loop principal con una Lista de prioridad. La prioridad la tiene la Tup con el menor tiempo variable
timePL :: StateT Env IO ()
timePL = do 
    ys <- get
    fstTime ys
    forever $ do
        xs <- get
        io $ check xs
        t <- getMinPQ
        diffAll t
        ans <- io (waitAndLook t)
        case ans of
            Left error -> do tick t
                             handleError t error
            Right (A n []) -> do setDefault t
                                 return ()
            Right (A n xs) -> do b <- checkOld (A n xs)
                                 case b of
                                     True -> do setDefault t
                                                return ()
                                     False -> do io $ okAnswer (fst' t) (A n xs)
                                                 setDefault t
                                                 putNewAns (A n xs)
                                                 return ()

--Maneja que hacer si hay un error, Si el error es CurlRecvError seteo el tiempo variable en 0 asi lo busca rapido 5 veces seguidas,
--si hay respuesta (vacia o no) listo y espera lo que tenga que esperar, si sigue el error a la 5ta la borro.
handleError :: Tup -> CurlCode -> StateT Env IO ()
handleError t err = do
    let n = fst' t
    case err of
        CurlRecvError -> if thd' t > 4 then deleteT t >> return () else io (errorT n err) >> setT t 0 >> return () --uso setT
        other -> do io $ errorT n other
                    deleteT t
                    return () 
                                
--Suma 1 a la cantidad de errores de la Tup dada
tick :: Tup -> StateT Env IO ()
tick t = do modify (\s -> tick' t s)
            return ()
         where tick' t [] = []
               tick' t (x:xs) = if fst' t == fst' x then (fst' x,snd' x,thd' x +1,fth' x):xs else x:(tick' t xs)

--Funcionas de Tupla de 3
fst' (a,_,_,_) = a
snd' (_,b,_,_) = b
thd' (_,_,c,_) = c
fth' (_,_,_,d) = d

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
    case n of
        Left error -> do tick x
                         handleError x error
                         fstTime xs
        Right (A np []) -> fstTime xs 
        Right (A np ys) -> do io $ okAnswer (fst' x) (A np ys)
                              putNewAns (A np ys)
                              fstTime xs

--Chequea si la respuesta anterior es la misma o no
checkOld :: Answer -> StateT Env IO Bool
checkOld ans = do
    xs <- get
    return $ f ans xs
        where f s [] = False
              f (A n ys) (x:xs) = if ys == fth' x then True else f (A n ys) xs

--Pone la respuesta nueva
putNewAns :: Answer -> StateT Env IO ()
putNewAns ans = do
    modify (\s -> putN ans s)
    return ()
        where putN ans [] = []
              putN q@(A n xs) ((n',t',e',rs):ys) = if n == (name n') then (n',t',e',xs):ys else (n',t',e',rs):(putN q ys)

--Espera y busca
waitAndLook :: Tup -> IO (Either CurlCode Answer)    
waitAndLook (n,t,e,rs) = do 
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
              diffA t ((n',t',e',rs):xs) = if fst' t == n' then (n',t',e',rs):(diffA t xs) else (n',t' - snd' t,e',rs):(diffA t xs)     

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

--Setea a default, y pone en 0 el numero de error, ya que 
--si llego hasta aca es que obtuvo una respuesta (vacia o no).
setDefault :: Tup -> StateT Env IO ()
setDefault t = do 
    modify (\s -> deflt t s)
    return ()
        where deflt t [] = []
              deflt t ((n,ti,e,rs):xs) = if fst' t == n then (n,time n,0,rs):xs else (n,ti,e,rs):(deflt t xs)  

--Setea el tiempo de la Tup al Int dado
setT :: Tup -> Int -> StateT Env IO ()
setT t n = do 
    modify (\s -> sett t n s)
    return ()
        where sett t n [] = []
              sett t n ((n',ti,e,rs):xs) = if fst' t == n' then (n',n,e,rs):xs else (n',ti,e,rs):(sett t n xs)  


