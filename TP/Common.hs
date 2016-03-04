module Common where

type Tags = String
--Tup es de la forma (Notificacion, Tiempo variable, Numero de veces con error, Respuesta anterior)
type Tup = (Notification,Int,Int,[String])
type Env = [Tup]

data Attr = Href
          | Text
          | Src
          | Id 
          | Class deriving (Show,Eq)

data NType = Log
           | Print 
           | Mail String deriving (Show,Eq)

data WaitT = T { hours :: Int
               , minutes :: Int } deriving (Show,Eq)

data Notification = N { name :: String
                      , time :: Int
                      , att :: Attr
                      , cond :: (Attr, String)
                      , tag :: Tags
                      , url :: String
                      , ntype :: NType } deriving (Show,Eq)

data Answer = A { nameN :: String
                , statements :: [String] } deriving Show



