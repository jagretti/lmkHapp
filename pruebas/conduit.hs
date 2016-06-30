{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child,
                        ($//), (&|), (&//), (>=>), attribute)
--import Language.Haskell.TH.Syntax
import Data.String
import Data.Time.Clock

-- The URL we're going to search
url = "https://hackage.haskell.org/package/http-conduit-2.1.10.1/docs/Network-HTTP-Simple.html"

-- The data we're going to search for
selectElem :: Cursor -> [Cursor]
selectElem = element "a"

-- Extract the data from each node in turn
extractText = T.concat . content

--extractAttr = T.concat . attribute "href"
extractAttr n = T.concat . attribute (fromString n)

-- Process the list of data elements
processData =  putStrLn . T.unpack . T.concat

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

-- test VER COMO HACER PARA CONTAR LOS TIEMPOS Y RESTAR LO QUE REALMENTE SE TARDA
main = do
    n <- getLine
    time <- getCurrentTime    
    print time
    cursor <- cursorFor url
    processData $ cursor $// selectElem &| extractAttr n
    time2 <- getCurrentTime
    print time2 
    print (diffUTCTime time2 time)




