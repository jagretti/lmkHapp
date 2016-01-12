module Notify where

import Common
import Find

import System.Console.Readline
import Data.Maybe

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
    name <- readK ">>"
    putStrLn "Ingrese tiempo de busqueda deseado en hs"
    t <- readK ">>"
    let time = read t :: Float
    putStrLn "Ingrese palabra a buscar"
    word <- readK ">>"
    putStrLn "Ingrese url"
    url <- readK ">>"
    return (N name time word url 0)

