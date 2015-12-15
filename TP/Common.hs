module Common where

import Data.Time

type Id = Int

data Comm = New 
          | Modify
          | List
          | Quit
          | Help
          | Delete

data Notify = N { name :: String
                , time :: Float
                , request :: String
                , url :: String } deriving Show

data Answer = A { times :: Int
                , statements :: [String] } deriving Show


