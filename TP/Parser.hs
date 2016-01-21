module Parser where

import Common
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char

untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          reservedNames = ["get","where","new","every","from"], opLetter = return ' ' })


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
    

parseNot :: Parser Notification
parseNot = do
    reserved untyped "new"
    name <- many1 letter
    newline
    spaces
    reserved untyped "get"
    attr <- parseAttr
    spaces
    reserved untyped "where"
    newline
    spaces
    attrC <- parseAttr
    reservedOp untyped "="
    cond <- many1 letter
    newline
    spaces
    reserved untyped "every"
    time <- digit
    newline
    spaces
    reserved untyped "from"
    url <- many1 letter
    return (N name (digitToInt time) attr (attrC,cond) url)


    

