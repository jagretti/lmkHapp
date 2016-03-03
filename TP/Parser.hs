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
                    space   
                    m <- manyTill anyChar (char ';') --MEJORAR!!!
                    return (Mail m)

parseTime :: Parser Int
parseTime = do x <- nat
               return $ fromIntegral x

parseTime' :: Parser WaitT
parseTime' = do try separator
                h <- nat
                char 'h'
                m <- nat
                char 'm'
                try separator
                return (T (fromIntegral h) (fromIntegral m)) 

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

--parseAll :: Parser [Notification]
--parseAll = do xs <- sepBy parseNot separator
--              return xs

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
    reserved untyped "new"
    name <- many1 letter
    separator
    reserved untyped "get"
    attr <- parseAttr <?> "id,text,class o src"
    separator
    reserved untyped "where"
    separator
    attrC <- parseAttr <?> "id,text,class o src"
    separator
    reservedOp untyped "="
    cond <- manyTill anyChar (char ';')
    separator
    tag <- reserved untyped "tag"
    separator
    reservedOp untyped "="
    ct <- manyTill anyChar (char ';')
    separator
    reserved untyped "every"
    time <- parseTime' 
    separator
    reserved untyped "type"
    ty <- parseNType <?> "log,print o mail"
    separator
    reserved untyped "from"
    url <- manyTill anyChar (char ';')
    return (N name (waitToMin time) attr (attrC,cond) ct url ty)
    

