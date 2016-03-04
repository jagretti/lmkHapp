module Main where

import Common
import Ghtml
import Pretty
import Parser
import Lookups
import Notification

import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad
import System.Environment
import Control.Monad.State
import Text.Parsec.Error


main :: IO ()
main = do
    args <- getArgs
    p <- parseFromFile parseAll (unwords args)
    case p of
        Left err -> print err
        Right xs -> do let list = map (\n -> (n,time n,0)) xs
                       runStateT timePQ list >> return () 

