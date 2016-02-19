module Notification where

import Common
import Ghtml
import Lookups
import Parser
import Pretty

import Control.Concurrent
import Control.Monad.State
import Data.List

type Error = String
type Tup = (Notification,Int)
type Env = [Tup]

n1 = N "ingreso" 0 Href (Text,"Horarios") "any" "http://web.fceia.unr.edu.ar/en/gacetillas/698-horarios-de-comisiones-de-ingreso-2016.html"
n5 = N "ole" 0 Text (Text,"Northcutt") "any" "http://www.ole.com.ar/"
n6 = N "nodejs" 0 Text (Text,"utilitarian") "any" "https://openclassrooms.com/courses/ultra-fast-applications-using-node-js/node-js-what-is-it-for-exactly"

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

timePQ :: StateT Env IO ()
timePQ = forever $ do 
    xs <- get
    io $ print (map (\x-> snd x) xs)
    t <- getMinPQ
    diffAll t
    ys <- get
    io $ print (map (\x-> snd x) ys)
    io $ do threadDelay $ (time (fst t)) * 100000
            t <- bigLookUp (fst t)
            case t of
                A n [] -> print "Vacio"
                A n s -> print s
    setDefault t
    

        
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
setDefault (n,t) = do 
    modify (\s -> map (\(n1,t1) -> if n == n1 then (n1,time n1) else (n1,t1)) s)
    return ()

main = runStateT timePQ [(n1,10),(n6,11),(n6,30)] >> return ()

{-
timePQ :: StateT Env IO ()
timePQ = do
    (x:xs) <- get
    diffFirst 
    io $ threadDelay $ (time (fst x)) * 100000
    io $ do r <- bigLookUp (fst x)
            case (statements r) of
                [] -> print "vacio"
                xs -> print xs
     
diffFirst :: StateT Env IO ()
diffFirst = do
    (x:xs) <- get
    ys <- mapM_ (\e -> (snd e) - (snd x)) xs
    return () 
-}    


{-
newtype QSMonad s = QSM { runQSM :: s -> IO (Either Error s) }

instance Monad QSMonad where
    return x = QSM (\s -> return $ Right s)
    h >>= f = QSM (\s -> do eith_v <- runQSM h s
                            case eith_v of
                                Right s' -> f s' -- Por que no funciona aca!!!
                                Left err -> return $ Left err) 
-}

{-
newtype QSMonad s = QSM { runQSM :: s -> (Either Error s) }

instance Monad QSMonad where
    return = QSM (\s -> Right s)
    h >>= f = QSM (\s -> do eith_v <- runQSM h
                            case eith_v of
                                Right s' -> runQSM (f s')
                                Left err -> return $ Left err) 

class Monad m => MonadQS m where
    goLast :: m ()
    showList :: m ()

instance MonadQS QSMonad where 
    goLast = 
-}
        
