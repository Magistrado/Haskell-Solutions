module CargaDatos(TablaDestino(..),
                  cargaArchivo,
                  cargarVueloManual) where

import qualified Vuelo as V
import qualified Data.Time.Clock as T
import Data.Char (toUpper)
import Control.Applicative
import System.IO.Error(catchIOError, isDoesNotExistError, tryIOError)
import Text.Read (readMaybe)

data TablaDestino = Salida | Entrada deriving (Show, Read)

cargaArchivo :: FilePath -> IO ([(Int, V.Vuelo)], [(Int, V.Vuelo)])
cargaArchivo path = do
    procesado <- tryIOError (readFile path)
    -- Use Data.Either (either)
    case procesado of
        Left l -> handlerCargaArchivo l >>= cargaArchivo
        Right r -> do
            let (vuelosLlegada, restante) = splitAt (read . head . lines $ r)
                                                $ (tail . lines) r
            let vuelosSalida = (fst . splitAt (read $ head restante))
                                        $ tail restante
            return (procesarVuelos vuelosLlegada,
                    procesarVuelos vuelosSalida)

--procesado <- fmap lines $ catchIOError (readFile $ path) handlerCargaArchivo --"vuelos.txt"

handlerCargaArchivo :: IOError -> IO String
handlerCargaArchivo e
    | isDoesNotExistError e =
        putStrLn "The file doesn't exists. Please, specify another file." >>
        putStr "Specify the path: " >> getLine
    | otherwise = ioError e

handlerRead :: Read a => String -> IO a
handlerRead mesg = do 
        elección <- getLine
        case readMaybe elección of
            Just x -> return x
            Nothing -> 
                putStrLn "ERROR. Invalid input." >>
                putStr mesg >> handlerRead mesg

-- Implementar el manejo de errores sobre read y IO
cargarVueloManual = do
    putStrLn "Introduzca la informacion solicitada"
    putStr "Numero de vuelo: "
    nroVuelo <- handlerRead "Numero de vuelo: " :: IO Int
    putStr "\nCodigo de la aerolinea: "
    codAeroLinea <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nCodigo del aeropuerto de partida: "
    codPuertoPartida <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nCodigo del aeropuerto de llegada: "
    codPuertoLlegada <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nHora de salida/llegada: "
    -- Implement Manejo de formato de hora
    hora <-  fmap (\horaStr -> convertirSegundos $
                                dividirDelim horaStr ':') getLine
    putStr "\nPuerta de salida/llegada: "
    puerta <- handlerRead "\nPuerta de salida/llegada: " :: IO Int
    putStr "\n¿Salida o Entrada? "
    destinoTabla <- handlerRead "\n¿Salida o Entrada? " :: IO TablaDestino
    putStr "Estado de vuelo: "
    estadoVuelo <- handlerRead "Estado de vuelo: " :: IO V.Estado
    return (destinoTabla,   
                V.crearVuelo nroVuelo codAeroLinea
                                codPuertoPartida codPuertoLlegada
                                (T.secondsToDiffTime hora)
                                puerta
                                estadoVuelo )

-- Use Applicative Functor
convertirSegundos :: [String] -> Integer
convertirSegundos hora = sum $ getZipList 
                            $ ZipList [(1*) . read, (60*) . read , (3600*) . read]
                                <*> ZipList hora

-- Implementar el manejo de errores sobre read
procesarVuelos = map fragmentar
    where
        fragmentar vuelo = (codVuelo , V.crearVuelo codVuelo
                                (fragmentado !! 1) (fragmentado !! 2)
                                (fragmentado !! 3)
                                ((T.secondsToDiffTime . convertirSegundos)
                                    formatoTiempo)
                                (read $ fragmentado !! 5)
                                (read $ fragmentado !! 6))
            where
                codVuelo = read $ head fragmentado
                fragmentado = dividirDelim vuelo ','
                
                formatoTiempo = dividirDelim (fragmentado !! 4) ':'


dividirDelim :: String -> Char -> [String]
dividirDelim lin delimitador = case dropWhile (delimitador==) lin of
                                "" -> []
                                s' -> w : dividirDelim s'' delimitador
                                      where (w, s'') =
                                             break (delimitador==) s'

