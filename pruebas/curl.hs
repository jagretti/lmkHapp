import Network.Curl (curlGetString)
import Network.Curl.Opts
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Data.List

main = do 
    putStrLn "URL"
    url <- getLine
    res <- curlGetString url []
--    tag <- parseTags (snd res)          
    pageT <- return $ parseTags (snd res)
--    let f = filter (\t -> isTagText t) pageT
--    let l = filter (tagText (isInfixOf "Parcial")) f
    print (tagTree pageT)    
