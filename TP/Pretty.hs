module Pretty where

import Common
import Ghtml

import System.Console.Readline
import Data.Maybe
import Data.List

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

blank :: [String] -> [String]
blank xs = filter (\x -> x /= "") xs

rpt :: [String] -> [String]
rpt [] = []
rpt [x] = [x]
rpt (x:y:xs) = if x == y then rpt (y:xs) else x:y:(rpt xs) 

cleanAnswer :: Answer -> Answer
cleanAnswer (A n s) = A n (rpt $ blank s)
