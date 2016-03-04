module Pretty where

import Common
import Ghtml

import Data.List
import Data.String.Utils

--Borra espacios en blanco (a veces pasa)
blank :: [String] -> [String]
blank xs = filter (\x -> x /= "") xs

--Limpia y deja solo una vez cuando la respuesta en la misma string repetida
rpt :: [String] -> [String]
rpt [] = []
rpt [x] = [x]
rpt (x:y:xs) = if x == y then rpt (y:xs) else x:y:(rpt xs) 

cleanAnswer :: Answer -> Answer
cleanAnswer (A n s) = A n (rpt $ blank $ map strip s)
