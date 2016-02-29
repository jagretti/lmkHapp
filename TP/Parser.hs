module Parser where

import Common
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Network.URI (parseURI, uriToString)

untyped :: TokenParser u
untyped = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                          reservedNames = ["get","where","new","every","from","tag","type"],
                                          opLetter = return ' ' })
                

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

parseNType :: Parser NType
parseNType = do string "print"
                return Print
             <|> do string "log"
                    return Log
             <|> do string "mail"
                    return Mail

parseTime :: Parser Int
parseTime = do x <- nat
               return $ fromIntegral x

separator :: Parser ()
separator = 
    spaces 
    <|> do newline
           return () 
    <|> do tab
           return ()
    <|> do spaces
           return ()

parseAll :: Parser [Notification]
parseAll = do xs <- sepBy parseNot separator
              return xs
               
parseNot :: Parser Notification
parseNot = do
    reserved untyped "new"
    name <- many1 letter
    separator
    reserved untyped "get"
    attr <- parseAttr <?> "Se esperaba id,text,class o src"
    separator
    reserved untyped "where"
    separator
    attrC <- parseAttr <?> "Se esperaba id,text,class o src"
    separator
    reservedOp untyped "="
    cond <- manyTill anyChar newline
    separator
    tag <- reserved untyped "tag"
    separator
    reservedOp untyped "="
    ct <- manyTill anyChar newline
    separator
    reserved untyped "every"
    time <- parseTime 
    separator
    reserved untyped "type"
    ty <- parseNType <?> "Se esperaba log,print o mail"
    separator
    reserved untyped "from"
    url <- manyTill anyChar newline
    return (N name time attr (attrC,cond) ct url ty)
    

