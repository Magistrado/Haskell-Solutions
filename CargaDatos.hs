module CargaDatos(TablaDestino(..),
                  cargaArchivo,
                  cargarVueloManual) where

import qualified Vuelo as V
import qualified Data.Time.Clock as T
import Data.Char (toUpper)
import Control.Applicative

data TablaDestino = Salida | Entrada deriving (Show, Read)

cargaArchivo path =
    do
        procesado <- fmap lines $ readFile path --"vuelos.txt"
        let (vuelosLlegada, restante) =
                splitAt ((read $ head procesado)) $ tail procesado
        let vuelosSalida = (fst . splitAt (read $ head restante)) $ tail restante

        return (procesarVuelos vuelosLlegada, procesarVuelos vuelosSalida)

cargarVueloManual = do
    putStrLn "Introduzca la informacion solicitada"
    putStr "Numero de vuelo: "
    nroVuelo <- fmap (\str -> read str :: Int) getLine
    putStr "\nCodigo de la aerolinea: "
    codAeroLinea <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nCodigo del aeropuerto de partida: "
    codPuertoPartida <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nCodigo del aeropuerto de llegada: "
    codPuertoLlegada <- fmap (\cod -> (map toUpper . take 3) cod) getLine
    putStr "\nHora de salida/llegada: "
    hora <-  fmap (\horaStr -> map (\str -> read str :: Integer) $
                                dividirDelim horaStr ':') getLine
    putStr "\nPuerta de salida/llegada: "
    puerta <- fmap (\str -> read str :: Int) getLine
    putStr "\n¿Salida o Entrada? "
    destinoTabla <- fmap (\str -> read str :: TablaDestino) getLine
    putStr "Estado de vuelo: "
    estadoVuelo <- fmap (\str -> read str :: V.Estado) getLine
    return (destinoTabla,
                V.crearVuelo nroVuelo codAeroLinea
                                codPuertoPartida codPuertoLlegada
                                (T.secondsToDiffTime (( (hora !! 0) * 60 * 60)
                                  + ((hora !! 1) * 60)
                                  + (hora !! 2)))
                                puerta
                                estadoVuelo )


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
                codVuelo = read $ fragmentado !! 0
                fragmentado = dividirDelim vuelo ','
                -- Use Applicative Functor
                convertirSegundos hora = sum $ getZipList $
                        ZipList [(1*) . read, (60*) . read , (3600*) . read]
                            <*> ZipList hora
                formatoTiempo = dividirDelim (fragmentado !! 4) ':'

-}

dividirDelim :: String -> Char -> [String]
dividirDelim lin delimitador = case dropWhile (delimitador==) lin of
                                "" -> []
                                s' -> w : dividirDelim s'' delimitador
                                      where (w, s'') =
                                             break (delimitador==) s'

