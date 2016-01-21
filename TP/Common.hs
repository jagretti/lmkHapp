module Common where


data Attr = Href
          | Text
          | Src
          | Id 
          | Class deriving Show

data Comm = New
          | Load 
          | Help
          | Quit

data Notification = N { name :: String
                      , time :: Int
                      , att :: Attr
                      , cond :: (Attr, String)
                      , url :: String } deriving Show

data Answer = A { nameN :: String
                , statements :: [String] } deriving Show


