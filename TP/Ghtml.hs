module Ghtml where

import Common

import Control.Concurrent
import Text.XML.HXT.Core
import Network.Curl
import Data.List

--Devuelve el archivo Html como una String de Haskell, o error si lo hubo
getPage :: URLString -> IO (Either CurlCode String)
getPage uri = do
    eresp <- curlGetString uri []
    case fst eresp of
        CurlOK -> return $ Right (snd eresp)
        _ -> return $ Left (fst eresp)

--Parsea la String del Html
parseXML :: String -> IOStateArrow s b XmlTree
parseXML st = do 
    readString [ withParseHTML      yes
               , withWarnings       no
               ] st

--Junto el trabajo de getPage y parseXML
getXML :: URLString -> IO (Either CurlCode (IOStateArrow s b XmlTree))
getXML url = do 
    doc <- getPage url
    case doc of
        Right s -> return $ Right (parseXML s)
        Left err -> return $ Left err

--Cambia el error por una string para poder mostrarselo al usuario
curlError :: CurlCode -> String
curlError err = case err of
                    CurlUnspportedProtocol -> "Error: Protocolo no soportado."
                    CurlCouldntConnect -> "Error: No se pudo conectar."
                    CurlCouldntResolveHost -> "Error: No se pudo resolver Host."
                    CurlHttpReturnedError -> "Error: Protocolo HTTP retorno error, verifique URL."
                    CurlRecvError -> "Error: Error de recepcion de datos."
                    _ -> "Error sin descripcion, verifique notificacion."
                    


