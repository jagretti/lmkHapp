module Main where

import Common
import Ghtml
import Pretty
import Parser
import Lookups

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.List
import Control.Monad
import System.Environment
import Data.Time.Clock
import Data.Time.Calendar

--comm = [(Load,"Escribe :l o :load seguido del nombre del archivo del archivo de notificacion"),
  --      (Help,"Se muestra la ayuda"),
    --    (Quit,"Salir")]

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
n12 = N "haskell" 0 Href (Text,"Full Source") "any" "http://dev.stephendiehl.com/fun/002_parsers.html"
n13 = N "real" 0 Id (Text,"JSON") "any" "http://book.realworldhaskell.org/read/using-parsec.html"
n14 = N "yahoo" 0 Text (Text,"Elecciones") "any" "https://espanol.yahoo.com/"
n15 = N "yahoo1" 0 Href (Text,"Yahoo") "any" "https://espanol.yahoo.com/"

main :: IO ()
main = do
    args <- getArgs
    p <- parseFromFile parseAll (unwords args)
    print p
    case p of
        Left err -> print err
        Right xs -> do l <- bigLookUp (xs !! 1)    
                       print l
{-    
    t <- date
    time <- getCurrentTime
    print $ utctDayTime time -- Esto da la hora en segundos
    print $ utctDayTime (addUTCTime 60 time) -- Esto suma segundos
-}
--date :: IO (Integer,Int,Int) -- :: (year,month,day)
--date = getCurrentTime >>= return . showGregorian . utctDay --Esto da la fecha exacta! Para escribir en el log o por pantalla


{-
welcome = do 
    putStrLn "--- Bienvenido!"
    putStrLn "--- Para ayuda escriba :help"

go :: IO ()
go = do
    c <- parseComm parseC
    case c of
        Right t -> case t of
                       Load s -> do result <- parseFromFile parseAll s
                                    case result of
                                        Left err -> print err
                                        Right xs -> print xs
                       Help -> do print "help"
                                  go
                       Quit -> return ()
        Left err -> print err
                         
-}








