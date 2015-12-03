import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import Text.HandsomeSoup
import Data.List

weatherDataURL = "http://www.fceia.unr.edu.ar/lcc/jcc/2015/"

retrieveWeatherData = do
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
            

main = do doc <- retrieveWeatherData
          xml <- return $ parseXML doc
          result <- runX (xml //> hasText (isInfixOf "Jornadas") >>> getText)
--          result <- runX (xml >>> css "body" >>> removeAllWhiteSpace //> getText)
          case result of
            [] -> putStrLn "no funco algo"
            w -> print w
