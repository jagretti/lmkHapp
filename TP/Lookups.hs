module Lookups where

import Common
import Ghtml
import Pretty

import Network.Curl
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Data.List

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
    return m

--Consigue Atributo con condicion de otro atributo
lookUpAtAt n page = do
    let at = translate $ att n
    t <- runX $ page >>> css ((tags (tag n))++"["++(translate $ fst (cond n))++"~="++(snd (cond n))++"]") ! at
    return t

--Consigue Texto con determinado Texto (cueck)
lookUpTT n page = do
    u <- runX $ page >>> css (tags (tag n)) >>> hasText (isInfixOf (snd $ cond n)) >>> getText
    return u

--Consigue Texto con determinado atributo
lookUpTAt n page = do
    u <- runX $ page >>> css ((tags (tag n))++"["++(translate $ fst (cond n))++"~="++(snd (cond n))++"]") //> getText
    return u

--Hace la seleccion de lookUps solo
bigLookUp :: Notification -> IO (Either CurlCode Answer)
bigLookUp n = do
    page <- getXML $ url n
    case page of
        Right p -> do ans <- go n p
                      return $ Right ans
        Left r -> return $ Left r

go n p = do
    let at = att n
    let atc = fst (cond n)
    case at of
        Text -> case atc of
                Text -> do x <- lookUpTT n p
                           return (f (A (name n) x))
                _ -> do x <- lookUpTAt n p
                        return (f (A (name n) x))
        _ -> case atc of
             Text -> do x <- lookUpAtT n p
                        return (f (A (name n) x))
             _ -> do x <- lookUpAtAt n p
                     return (f (A (name n) x))
    where f = cleanAnswer



