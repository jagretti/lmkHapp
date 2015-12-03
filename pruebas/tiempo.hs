import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Text.HandsomeSoup
import Data.List
import Data.Algorithm.Diff
import Control.Concurrent
import Control.Concurrent.MVar

--weatherDataURL = "http://espanol.weather.com/weather/today/Rosario+SF+Argentina+ARSF0078:1:AR"
--weatherDataURL = "http://globoesporte.globo.com/"

retrieveWeatherData weatherDataURL = do
                        case parseURI weatherDataURL of
                            Nothing  -> ioError . userError $ "Invalid URL"
                            Just uri -> get uri

get uri = do
  eresp <- simpleHTTP (Request uri GET [] "")
  case eresp of
    Left _    -> ioError . userError $ "Failed to get " ++ show uri
    Right res -> return $ rspBody res 

parseXML doc = readString [ withValidate no
                          , withRemoveWS yes
                          , withParseHTML yes
                          , withWarnings no
                          ] doc

func w weatherDataURL = do
        doc <- retrieveWeatherData weatherDataURL
        xml <- return $ parseXML doc
--    result <- runX (xml /> deep(hasText (isInfixOf "huracan")))
--    result <- runX (xml >>> hasText(isInfixOf "huracan"))
        result <- runX (xml //> hasText (isInfixOf w) >>> getText)
{-         case concat result of
            [] -> putStrLn "no funco algo"
            w -> print w-}
        return result                                 

difference :: String -> String -> Int
difference w1 w2 = undefined

main = do 
    putStrLn "Ingrese la URL de la pagina"
    weatherDataURL <- getLine
    putStrLn "Ingrese la palabra a buscar"
    w <- getLine

--pruebas raras, algunas funcan otras creo que no
{-    doc <- retrieveWeatherData weatherDataURL
    xml <- return $ parseXML doc
--    result <- runX (xml /> deep(hasText (isInfixOf "huracan")))
--    result <- runX (xml >>> hasText(isInfixOf "huracan"))
    result <- runX (xml //> hasText (isInfixOf ) >>> getText)
    case concat result of
         [] -> putStrLn "no funco algo"
         w -> print w
-}
    v <- func w weatherDataURL
    print v

--esto tira error de tipo! ver por que carajo
{-    m <- newEmptyMVar
    forkIO (putMVar m (func w weatherDataURL))
    r <- takeMVar m
    print r
-}
--ver MVar para comunicar los threads!!!! asi se imprime lo que hace un thread en el thread principal.
    
    



