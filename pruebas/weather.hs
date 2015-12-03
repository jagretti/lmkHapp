{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Network.HTTP
import Network.URI

weatherDataURL = "http://www.weather.gov/xml/current_obs/KAGC.xml"

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
                          ] doc

data Weather = Weather 
  { location, observationTime,
    summary, windDirection :: String,

    temperature, humidity, 
    dewpoint,
    pressure, windSpeed, 
    visibility             :: Float }
  deriving (Eq, Show)

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

getWeather = atTag "current_observation" >>>
  proc x -> do
    loc     <- textAtTag "location"          -< x
    obsTime <- textAtTag "observation_time"  -< x
    summ    <- textAtTag "weather"           -< x
    windDir <- textAtTag "wind_dir"          -< x
    temp    <- textAtTag "temp_c"            -< x
    humi    <- textAtTag "relative_humidity" -< x
    wind    <- textAtTag "wind_mph"          -< x
    pres    <- textAtTag "pressure_mb"       -< x
    dew     <- textAtTag "dewpoint_c"        -< x
    vis     <- textAtTag "visibility_mi"     -< x
    returnA -< Weather 
      { location        = loc,
        observationTime = obsTime,
        summary         = summ,
        windDirection   = windDir,
        temperature     = read temp,
        humidity        = read humi,
        windSpeed       = read wind * 1.61,
        pressure        = read pres,
        dewpoint        = read dew,
        visibility      = read vis * 1.61 }

-- GHCi test:
-- Main> retrieveWeatherData >>= \ doc -> runX (parseXML doc >>> getWeather)

main = do
  doc    <- retrieveWeatherData
  xml    <- return $ parseXML doc
  result <- runX (xml >>> getWeather)
  case result of
    []  -> putStrLn "Unable to parse weather data."
    w:_ -> print w
