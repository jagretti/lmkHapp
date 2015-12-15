module Notify where

import Common
import Find

import System.Console.Readline
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe

type List = [(Notify,Answer)]
{-
newtype State = State { runState :: IO List }

instance Monad State where
    return x = State . return
    m >>= f = (\l -> 
-}
toSeconds :: Float -> Float
toSeconds x = x*3600

sleep :: Float -> IO ()
sleep n = threadDelay $ round $ (toSeconds n)*1000000

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
    name <- readK ">"
    putStrLn "Ingrese tiempo de busqueda deseado en hs"
    t <- readK ">"
    let time = read t :: Float
    putStrLn "Ingrese palabra a buscar"
    word <- readK ">"
    putStrLn "Ingrese url"
    url <- readK ">"
    return (N name time word url)

