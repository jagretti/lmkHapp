module Parser where

import Common
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Network.URI (parseURI, uriToString)

untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          reservedNames = ["get","where","new","every","from","tag"],
                                          opLetter = return ' ' })

parseComm :: Parser a -> IO (Either ParseError a)
parseComm p = do
    c <- getLine
    return (parse p "" c)

parseC :: Parser Comm
parseC = do
    try $ string ":load"
    try space
    fileP <- manyTill anyChar eof
    return $ Load fileP
    <|> do
    try $ string ":help"
    return Help
    <|> do
    string ":quit"
    return Quit
                

parseAttr :: Parser Attr
parseAttr = do string "href"
               return Href
            <|> do string "id"
                   return Id
            <|> do string "text"
                   return Text
            <|> do string "class"
                   return Class
            <|> do string "src"
                   return Src

separator :: Parser ()
separator = 
    spaces 
    <|> do newline
           return () 
    <|> do tab
           return ()



parseNot :: Parser Notification
parseNot = do
    reserved untyped "new"
    name <- many1 letter
    separator
    reserved untyped "get"
    attr <- parseAttr
    separator
    reserved untyped "where"
    separator
    attrC <- parseAttr
    space
    reservedOp untyped "="
    cond <- many1 letter
    separator
    tag <- many1 letter
    space
    reservedOp untyped "="
    ct <- many1 letter
    separator
    reserved untyped "every"
    time <- digit
    separator
    reserved untyped "from"
    url <- manyTill anyChar newline
    return (N name (digitToInt time) attr (attrC,cond) ct url)
    

