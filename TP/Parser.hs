module Parser where

import Common
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Network.URI (parseURI, uriToString)

names :: TokenParser u
names = makeTokenParser (haskellStyle { identStart = letter <|> char '_',
                                        reservedNames = ["get","where","new","every","from","tag","type"],
                                        opLetter = return ' ' })
                

parseAttr :: Parser Attr
parseAttr = do try (do string "href"
                       return Href
                    <|> do string "id"
                           return Id
                    <|> do string "text"
                           return Text
                    <|> do string "class"
                           return Class
                    <|> do string "src"
                           return Src)
            <|> fail "Fallo, revise el atributo pedido"
           

parseNType :: Parser NType
parseNType = do try (do string "print"
                        char ';'
                        return Print
                    <|> do string "log"
                           char ';'
                           return Log
                    <|> do string "mail"
                           space   
                           m <- parseAny
                           return (Mail m))
             <|> fail "Fallo, revise el Tipo de la notificacion"

parseAny :: Parser String
parseAny = do try (do x <- manyTill anyChar (char ';')
                      return x)
           <|> fail "Fallo, revise la notificacion"

parseTime' :: Parser WaitT
parseTime' = do try (do try separator
                        h <- nat
                        char 'h' <?> "h"
                        m <- nat
                        char 'm' <?> "m"
                        char ';'
                        try separator
                        return (T (fromIntegral h) (fromIntegral m)))
             <|> fail "Fallo, revise el tiempo dado"

--Pasa de WaitT a tiempo en MINUTOS
waitToMin :: WaitT -> Int
waitToMin (T h m) = h*60+m

--Parsea espacios, tabs, \n
separator :: Parser ()
separator = 
    spaces 
    <|> do newline
           return () 
    <|> do tab
           return ()
    <|> do spaces
           return ()

--Parser de todo un archivo conf con notificaciones
parseAll = do ys <- do try separator
                       x <- try parseNot
                       try separator
                       xs <- try parseAll
                       return (x:xs)
              return ys                   
           <|> do try eof 
                  return []

--Parsea una notificacion            
parseNot :: Parser Notification
parseNot = do
    reserved names "new"
    name <- many1 letter   --El nombre de las notificaciones solo puede ser en Letras
    separator
    reserved names "get"  <?> "get"
    attr <- parseAttr  <?> "id,text,class,href o src"
    separator
    reserved names "where" <?> "where"
    separator
    attrC <- parseAttr  <?> "id,text,class o src"
    separator
    reservedOp names "=" 
    cond <- parseAny         
    separator
    tag <- reserved names "tag" <?> "tag"
    separator
    reservedOp names "=" 
    ct <- parseAny           
    separator
    reserved names "every" <?> "every"
    time <- parseTime'
    separator
    reserved names "type" <?> "type"
    ty <- parseNType <?> "log,print o mail"
    separator
    reserved names "from" <?> "from"
    url <- parseAny          
    return (N name (waitToMin time) attr (attrC,cond) ct url ty)
    

