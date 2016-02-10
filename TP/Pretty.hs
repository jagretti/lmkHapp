module Pretty where

import Common
import Ghtml

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


