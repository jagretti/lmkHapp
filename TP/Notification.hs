module Notification where

import Common
import Ghtml
import Lookups
import Parser
import Pretty

import Control.Concurrent
import Control.Monad.State
import Data.List
import Language.Haskell.TH.Syntax
import Control.Error
import Control.Monad
import Control.Monad.Trans
import System.Exit

type Error = String
type Tup = (Notification,Int)
type Env = [Tup]

n1 = N "ingreso" 10 Href (Text,"Horarios") "any" "http://web.fceia.unr.edu.ar/en/gacetillas/698-horarios-de-comisiones-de-ingreso-2016.html"
n5 = N "ole" 11 Text (Text,"Northcutt") "any" "http://www.ole.com.ar/"
n6 = N "nodejs" 30 Text (Text,"utilitarian") "any" "https://openclassrooms.com/courses/ultra-fast-applications-using-node-js/node-js-what-is-it-for-exactly"

{-
newtype QSMonad a = QSM { runQSM :: Env -> Either Error (a,Env) }

instance Monad QSMonad where
    return x = QSM (\s -> Right (x,s))
    QSM h >>= f = QSM (\s -> case h s of
                                 Left err -> Left err
                                 Right (a,s') -> runQSM (f a) s')

class Monad m => MonadState m where
    getMin :: m (Notification,Int) 
    diffAll :: Tup -> m ()
    setDefault :: Tup -> m () 

instance MonadState QSMonad where
    getMin = QSM (\s -> findMinL s >>= (\x -> Right (x,s)))
                where findMinL [] = Left "Error"
                      findMinL [x] = Right x
                      findMinL (x:y:xs) = if snd x <= snd y then findMinL (x:xs) else findMinL (y:xs)
    diffAll t = QSM (\s -> Right ((),f t s)) 
                   where f t [] = []
                         f (n,t) ((n1,t1):xs) = if n == n1 then f (n,t) xs else (n1,t1-t):(f (n,t) xs)    
-}


io :: IO a -> StateT Env IO a
io = liftIO

check xs = when (xs == []) exitSuccess

timePQ :: StateT Env IO ()
timePQ = forever $ do 
    xs <- get
--    io $ print (map (\x-> snd x) xs)
    io $ check xs
    t <- getMinPQ
    diffAll t
    ys <- get
--    io $ print (map (\x-> snd x) ys)
    ans <- io (waitAndLook t)
    case ans of
        A n [] -> do setDefault t
--                     io $ print "no encontre nada, tengo que esperar" 
                     return ()
        A n xs -> do io (print xs)
                     deleteT t
    
waitAndLook (n,t) = do 
    threadDelay $ t * 60000000
    r <- bigLookUp n
    return r
        
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

--main = runStateT timePQ [(n1,10),(n5,11),(n6,30)] >> return () 

        
