module Main where

import Common
import Ghtml
import Notification
import Parser
import Lookups

import Control.Concurrent
import Control.Concurrent.MVar
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Data.List

--comands = 

n1 = N "ingreso" 0 Class (Text,"Horarios") "http://web.fceia.unr.edu.ar/en/gacetillas/698-horarios-de-comisiones-de-ingreso-2016.html"
n2 = N "Diario-Sport" 0 Href (Text,"marca") "http://www.sport.es/es/noticias/barca/dia-que-cristiano-ronaldo-hizo-traductor-messi-neymar-4828356"
n3 = N "w3school" 0 Href (Text,"LEARN") "http://www.w3schools.com/default.asp"
n4 = N "js" 0 Class (Text,"Backbone") "http://fernetjs.com/"
n5 = N "ole" 0 Text (Text,"resentimientos") "http://www.ole.com.ar/"
n6 = N "nodejs" 0 Text (Text,"utilitarian") "https://openclassrooms.com/courses/ultra-fast-applications-using-node-js/node-js-what-is-it-for-exactly"
n7 = N "w3" 0 Text (Class,"w3-right toptext w3-wide") "http://www.w3schools.com/default.asp"

main :: IO ()
main = do
{-
   result <- parseFromFile parseNot "origen.txt" 
   case result of
       Left err -> print err
       Right xs -> print xs
-}
    page <- getXML (url n7)
--    putStrLn "Busqueda 1"
--    lookUpOne n5 page
    putStrLn "Busqueda 2"
    lookUpThree n7 page





