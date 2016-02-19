import Network.Curl (curlGetString)
import Network.Curl.Opts
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Data.List

main = do 
    putStrLn "Ingrese URL"
    url <- getLine
    res <- curlGetString url []
    pageT <- return $ parseTags (snd res)
    print pageT    
