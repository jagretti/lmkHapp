module TypeN where

import Common
import Ghtml
import Mail

import System.IO
import Data.Time
import Network.Curl

--getCurrentTime me retorna una hora con 3 horas de mas
timeCurrent :: IO UTCTime
timeCurrent = do t <- getCurrentTime
                 let t1 = addUTCTime (-10800) t
                 return t1

--Escribe Answer en un archivo .log
inFile :: Notification -> Answer -> IO ()
inFile n a = do
    let nn = name n
    let nfile = nn++".log"
    t <- timeCurrent
    appendFile nfile ("Nombre: "++nn++" | Respuesta: "++show(statements a)++" | Hora: "++show t++"\n")

--Imprime por pantalla el Answer
printIt :: Notification -> Answer -> IO ()
printIt n a = do
    let nn = name n
    t <- timeCurrent
    putStrLn $ "Nombre: "++nn++" | Respuesta: "++show(statements a)++" | Hora: "++show t

--Elige como mostrarle al usuario la respuesta
okAnswer :: Notification -> Answer -> IO ()
okAnswer n a = do
    case (ntype n) of   
        Log -> inFile n a
        Print -> printIt n a
        Mail m -> sendMail m ("Notificacion : "++name n) ("Respuesta: \n"++show (statements a)) 

--Elige como mostrarle al usuario el error, Si hay un error de una notificacion que se tiene que mandar por mail, solo se muestra por pantalla
errorT :: Notification -> CurlCode -> IO ()
errorT n err = do
    let nn = name n
    let nfile = nn++".log"
    t <- timeCurrent
    case (ntype n) of
        Log -> appendFile nfile ("ERROR: "++nn++" || "++curlError err++" "++show t++"\n")
        _ -> putStrLn $ "ERROR: "++nn++" || "++curlError err++" || "++show t --No mando al mail si hay error!
        
