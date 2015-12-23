module Notify where

import Common
import Find

import System.Console.Readline
import Data.Maybe
{-
type List = [(Notify,Answer)]

newtype State = State { runState :: List -> IO List }

instance Monad State where
    return x = State (\l -> return l)
    m >>= f = State (\l -> let v = runState l m
                           in runState v f)
                            
class Monad m => MonadState m where
    lookA :: Notify -> m Answer
--    tick :: Notify -> m ()

instance MonadState State where
    lookA n = State (\l -> lookfor' n l)
                    where lookfor' w (x:xs) | w == fst x = snd x
                                            | w /= fst x = lookfor' w xs
-}



readK :: String -> IO String
readK s = do
    t <- readline s
    case t of
        Nothing -> do putStrLn "Intente nuevamente"
                      readK s
        Just a -> case a of
                      [] -> do putStrLn "Intente nuevamente"
                               readK s
                      _ -> return a 


createNotify :: IO Notify
createNotify = do 
    putStrLn "Ingrese Nombre de la Notificacion"
    name <- getLine
    putStrLn "Ingrese tiempo de busqueda deseado en hs"
    t <- getLine
    let time = read t :: Float
    putStrLn "Ingrese palabra a buscar"
    word <- getLine
    putStrLn "Ingrese url"
    url <- getLine
    return (N name time word url 0)

