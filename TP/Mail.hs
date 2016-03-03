{-# LANGUAGE OverloadedStrings #-}

module Mail where

import qualified Data.Text.Lazy
import qualified Data.Text
import Network.Mail.Mime
import Network.Mail.Client.Gmail

sendMail :: String -> String -> String -> IO ()
sendMail mail n xs = sendGmail "notificationlmk" "aplicacionlmk" (Address (Just "Let Me") "notificationlmk@gmail.com") [Address (Just "LMK") (Data.Text.pack mail)] [] [] (Data.Text.pack n) (Data.Text.Lazy.pack xs) [] 25000000
