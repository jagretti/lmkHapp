module Main where

import Common
import Ghtml
import Notification
--import Parser
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

n1 = N "ingreso" 0 Href (Text,"Horarios") "any" "http://web.fceia.unr.edu.ar/en/gacetillas/698-horarios-de-comisiones-de-ingreso-2016.html"
n2 = N "Diario-Sport" 0 Href (Text,"marca") "any" "http://www.sport.es/es/noticias/barca/dia-que-cristiano-ronaldo-hizo-traductor-messi-neymar-4828356"
n3 = N "w3school" 0 Href (Text,"LEARN") "any" "http://www.w3schools.com/default.asp"
n4 = N "js" 0 Href (Text,"Backbone") "any" "http://fernetjs.com/"
n5 = N "ole" 0 Text (Text,"Northcutt") "any" "http://www.ole.com.ar/"
n6 = N "nodejs" 0 Text (Text,"utilitarian") "any" "https://openclassrooms.com/courses/ultra-fast-applications-using-node-js/node-js-what-is-it-for-exactly"
n7 = N "w3" 0 Text (Href,"/colors/default.asp") "any" "http://www.w3schools.com/default.asp"
n8 = N "github" 0 Class (Id,"start-of-content") "any" "https://github.com/vhf/free-programming-books"
n9 = N "handsome" 0 Text (Id,"site-description") "div" "https://fateswanderer.wordpress.com/2013/11/17/parsing-with-handsomesoup/"
n10 = N "hackage" 0 Href (Text,"processTopDown") "any" "https://hackage.haskell.org/package/hxt-9.3.1.15/docs/Text-XML-HXT-Arrow-XmlArrow.html"
n11 = N "olee" 0 Href (Text,"contactanos@ole.com.ar") "any" "http://www.ole.com.ar/"

main :: IO ()
main = do
{-
   result <- parseFromFile parseNot "origen.txt" 
   case result of
       Left err -> print err
       Right xs -> print xs
-}
--    page <- getXML (url n9)
--    putStrLn "Busqueda 1"
--    lookUpOne n5 page
    bigLookUp n5 (url n5)
--    lookUpTAt n9 page





