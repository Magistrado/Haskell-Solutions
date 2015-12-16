module CargaDatos(cargaArchivo) where

import qualified Vuelo as V
import qualified Data.Time.Clock as T

cargaArchivo path =
    do
        procesado <- fmap lines $ readFile path --"vuelos.txt"
        let (vuelosLlegada, restante) =
                splitAt ((read $ head procesado)) $ tail procesado
        let vuelosSalida = (fst . splitAt (read $ head restante)) $ tail restante

        return (procesarVuelos vuelosLlegada, procesarVuelos vuelosSalida)






procesarVuelos = map fragmentar
    where
        fragmentar vuelo = (codVuelo , V.crearVuelo codVuelo
                                (fragmentado !! 1) (fragmentado !! 2)
                                (fragmentado !! 3)
                                (T.secondsToDiffTime ((horas * 60 * 60)
                                  + (minutos * 60)
                                  + segundos))
                                (read $ fragmentado !! 5)
                                (read $ fragmentado !! 6))
            where
                codVuelo = read $ fragmentado !! 0
                fragmentado = dividirDelim vuelo ','
                formatoTiempo = dividirDelim (fragmentado !! 4) ':'
                horas = read (formatoTiempo !! 0) :: Integer
                minutos = read (formatoTiempo !! 1) :: Integer
                segundos = read (formatoTiempo !! 2) :: Integer



dividirDelim :: String -> Char -> [String]
dividirDelim lin delimitador = case dropWhile (delimitador==) lin of
                                "" -> []
                                s' -> w : dividirDelim s'' delimitador
                                      where (w, s'') =
                                             break (delimitador==) s'

