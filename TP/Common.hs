module Common where

import Data.Time

type Id = Int

data Comm = New 
          | Quit
          | Help

data Notify = N { name :: String
                , time :: Float
                , request :: String
                , url :: String 
                , num :: Int } deriving (Show,Ord,Eq)

data Answer = A { times :: Int
                , statements :: [String] } deriving Show


