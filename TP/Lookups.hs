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

--CONSIGUE Attr (menos Text) CON DETERMINADO TEXTO VISIBLE                
lookUpOne n page = do
    let at = translate $ att n
--    t <- runX $ page //> hasAttr at >>> (deep (hasText (isInfixOf (snd $ cond n)))) &&& getAttrValue at   --Intento para que solo salga el Attr deseado 
    t <- runX $ page //> hasAttr at >>> ((deep (hasText (isInfixOf (snd $ cond n))) >>> getText) &&& getAttrValue at)
    print t

--CONSIGUE TEXTOS QUE DIGAN DETERMINADA COSA
lookUpTwo n page = do
    u <- runX $ page //> hasText (isInfixOf (snd $ cond n)) >>> getText
    print u


lookUpThree n page = do
    let t1 = translate (fst $ cond n)
    let t2 = cond n
    u <- runX $ page >>> deep (hasAttrValue "class" (=="w3-right toptext w3-wide")) //> getText
    print u

--main = do
{-
    t <- getTime $ Monotonic
    threadDelay $ 60 * 1000000
    t' <- getTime $ Monotonic
    print (sec t' - sec t)
    

    url <- getLine
    page <- getXML url
    t <- runX $ page //> hasAttr "href" >>> (deep (hasText (isInfixOf "Stream"))) &&& getAttrValue "href" -- consigue href con determinado texto y el mapM imprime solo href
    mapM_ (\x -> putStrLn (snd x)) t
    putStrLn "href con determinado texto"
    print t

    t <- runX $ page //> hasAttr "href" >>> ((deep (hasText (isInfixOf "Stream")) >>> getText) &&& getAttrValue "href") -- consigue href con determinado texto

    u <- runX $ page //> hasText (isInfixOf "Parcial") >>> getText -- consigue el texto que diga tal cosa
    putStrLn "texto con determinado texto"
    print u 
-}    
    









   

