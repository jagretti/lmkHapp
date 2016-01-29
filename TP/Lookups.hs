module Lookups where

import Common
import Ghtml
import Notification

import Network.Curl
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Data.List
import System.Clock
import Control.Concurrent

translate :: Attr -> String
translate a = case a of
            Href -> "href"
            Class -> "class"
            Src -> "src"
            Id -> "id"
            Text -> "text"

tags :: Tags -> String
tags t = case t of
         "any" -> "*"
         xs -> xs

--Consigue Attr (menos Text) con determinado texto visible               
lookUpAtT n page = do
    let at = translate $ att n
    t <- runX $ page >>> css (tags (tag n)) >>> hasAttr at >>> (deep (hasText (isInfixOf (snd $ cond n))) &&& getAttrValue at)
    let m = map (\x -> snd x) t
    print m

--Consigue Atributo con condicion de otro atributo
lookUpAtAt n page = do
    let at = translate $ att n
    t <- runX $ page >>> css ((tags (tag n))++"["++(translate $ fst (cond n))++"~="++(snd (cond n))++"]") ! at
    print t

--Consigue Texto con determinado Texto (cueck)
lookUpTT n page = do
    u <- runX $ page >>> css (tags (tag n)) //> hasText (isInfixOf (snd $ cond n)) >>> getText
    print u

--Consigue Texto con determinado atributo
lookUpTAt n page = do
    u <- runX $ page >>> css ((tags (tag n))++"["++(translate $ fst (cond n))++"~="++(snd (cond n))++"]") //> getText
    print u

--Hace la seleccion de lookUps solo
bigLookUp n url = do
    page <- getXML url
    let at = att n
    let atc = fst (cond n)
    case at of
        Text -> case atc of
                    Text -> lookUpTT n page
                    _ -> lookUpTAt n page
        _ -> case atc of
                Text -> lookUpAtT n page
                _ -> lookUpTT n page
         


