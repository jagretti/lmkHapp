module Common where

type Tags = String

data Attr = Href
          | Text
          | Src
          | Id 
          | Class deriving Show

data Comm = Load String
          | Help
          | Quit deriving Show

data Notification = N { name :: String
                      , time :: Int
                      , att :: Attr
                      , cond :: (Attr, String)
                      , tag :: Tags
                      , url :: String } deriving Show

data Answer = A { nameN :: String
                , statements :: [String] } deriving Show


