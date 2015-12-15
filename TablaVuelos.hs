module TablaVuelos where

import qualified Data.Map.Lazy as Map
import Vuelo (Vuelo)
import Control.Monad.State

type TablaVuelos = Map.Map Int Vuelo

type Central a = State Tabla a

newtype Tabla = Tab {
    obtener :: (TablaVuelos, TablaVuelos)
} deriving (Show)


crearTablaVuelos :: [(Int,Vuelo)] -> [(Int,Vuelo)] -> Tabla
crearTablaVuelos lL lS = Tab $ (Map.fromList lL, Map.fromList lS )

consultarVueloSalida, consultarVueloEntrada :: Int -> Central (Maybe Vuelo)
consultarVueloSalida codVuelo =
    get >>= (\centro -> return $ Map.lookup codVuelo (snd $ obtener centro))

consultarVueloEntrada codVuelo = do
    centro <- get
    return $ Map.lookup codVuelo (fst $ obtener centro)

consultarVueloSalidaImp :: Int -> Central String
consultarVueloSalidaImp codVuelo = do
    centro <- get
    case Map.lookup codVuelo (snd $ obtener centro)of
        Just vu -> return $ show vu
        Nothing -> return "Vuelo no encontrado"

imprimirTablaEntrada :: Central [String]
imprimirTablaEntrada = do
    central <- get
    return $ (map (show . snd) . Map.toDescList . fst . obtener) central

imprimirTablaSalida :: Central [String]
imprimirTablaSalida = do
    central <- get
    return $ (map (show . snd) . Map.toDescList . snd . obtener) central
