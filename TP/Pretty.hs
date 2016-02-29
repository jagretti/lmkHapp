module Pretty where

import Common
import Ghtml

import Data.Maybe
import Data.List

blank :: [String] -> [String]
blank xs = filter (\x -> x /= "") xs

rpt :: [String] -> [String]
rpt [] = []
rpt [x] = [x]
rpt (x:y:xs) = if x == y then rpt (y:xs) else x:y:(rpt xs) 

cleanAnswer :: Answer -> Answer
cleanAnswer (A n s) = A n (rpt $ blank s)
