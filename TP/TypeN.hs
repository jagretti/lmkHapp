module TypeN where

import Common
import Ghtml
import Mail

import System.IO
import Data.Time
import Network.Curl


--Escribe Answer en un archivo .log
inFile :: Notification -> Answer -> IO ()
inFile n a = do
    let nn = name n
    let nfile = nn++".log"
    t <- getCurrentTime
    appendFile nfile ("Nombre: "++nn++" | Respuesta: "++show(statements a)++" | Hora: "++show t++"\n")

--Imprime por pantalla el Answer
printIt :: Notification -> Answer -> IO ()
printIt n a = do
    let nn = name n
    t <- getCurrentTime
    putStrLn $ "Nombre: "++nn++" | Respuesta: "++show(statements a)++" | Hora: "++show t

--Elige como mostrarle al usuario la respuesta
okAnswer :: Notification -> Answer -> IO ()
okAnswer n a = do
    case (ntype n) of   
        Log -> inFile n a
        Print -> printIt n a
        Mail m -> sendMail m ("Notificacion : "++name n) ("Respuesta: \n"++show (statements a)) --MEJORAR!!

--Elige como mostrarle al usuario el error, Si hay un error de una notificacion que se tiene que mandar por mail, solo se muestra por pantalla
errorT :: Notification -> CurlCode -> IO ()
errorT n err = do
    let nn = name n
    let nfile = nn++".log"
    t <- getCurrentTime
    case (ntype n) of
        Log -> appendFile nfile ("ERROR: "++nn++" || "++curlError err++" "++show t++"\n")
        _ -> putStrLn $ "ERROR: "++nn++" || "++curlError err++" || "++show t --No mando al mail si hay error!
        
