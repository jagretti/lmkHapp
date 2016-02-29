module Common where

type Tags = String

data Attr = Href
          | Text
          | Src
          | Id 
          | Class deriving (Show,Eq)

data NType = Log
           | Print 
           | Mail deriving (Show,Eq)

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



