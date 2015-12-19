module Vuelo(Estado,
             Vuelo,
             crearVuelo,
             nroVuelo) where

import qualified Data.Time.Clock as T
--import qualified Data.ByteString.Lazy as B


-- Probar ByteString
data Estado = A_Tiempo
            | Retrasado
            | Cancelado
            | Abordando
            | Desembarcando
            deriving (Eq, Show, Read)

data Vuelo = Vuel {
                nroVuelo :: Int,
                codAerolinea :: String,
                codAeroSalida :: String,
                codAeroLlegada :: String,
                hora :: T.DiffTime,
                puerta :: Int,
                estado :: Estado
            } deriving (Eq)

instance Show Vuelo where
    show v = (show . nroVuelo) v ++ "  " ++  (show . codAerolinea) v ++ "  "
                ++ (show . codAeroSalida) v ++ "  " ++ (show . codAeroLlegada) v ++
                    "  " ++ (show . calcularHora . hora) v ++ "  " ++ (show . puerta) v ++
                    "  " ++ (show . estado) v

calcularHora :: T.DiffTime -> String
calcularHora tiempo' = show hora ++ ":" ++ show minutos ++ ":" ++ show segundos
    where --floor $ toRational dt.
        tiempo = floor $ toRational tiempo'
        segundos =  tiempo `rem` 60
        minutos = (tiempo `quot` 60) `rem` 60
        hora = tiempo `quot` 3600

crearVuelo :: Int -> String -> String -> String -> T.DiffTime -> Int -> Estado -> Vuelo
crearVuelo nroV codAl codAS codALl hora puerta estado
    = Vuel nroV codAl codAS codALl hora puerta estado


