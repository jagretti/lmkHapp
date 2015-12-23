import Network.Curl (curlGetString)
import Network.Curl.Opts
import Text.XML.HXT.TagSoup
import Text.HTML.TagSoup

main = do 
    putStrLn "URL"
    url <- getLine
    res <- curlGetString url []
--    tag <- parseTags (snd res)          
    print $ parseTags (snd res)      
          
