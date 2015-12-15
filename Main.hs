module Main where

import CargaDatos(cargaArchivo)
import Control.Monad.State
import qualified TablaVuelos as T
import System.Exit

menu = do
    putStrLn $ "PIZARRA DE VUELOS AEROPUERTO INTERNACIONAL SIMON BOLIVAR\n\n" ++
                "Eliga una opción:"
    putStr $ "1) Ver pizarra\n" ++ "2) Cargar vuelos desde archivo\n" ++
                "3) Cargar vuelo manualmente\n" ++ "4) Eliminar vuelo\n"
                ++ "0) Salir\n" ++ "Su elección: "
    elección <- getChar
    return ()
    -- Usar una excepcion para asegurar que sea un digito
    {-case (read elección : Int) of
        1 -> putStrLn "Pizarra de vuelos de entrada" >>
                evalState T.imprimirTablaEntrada centro >>
                    putStrLn "Pizarra de vuelos de salida" >>
                        evalState T.imprimirTablaSalida centro

        2 -> undefined
        otherwise -> exitSucess
-}



main = do
    putStrLn $ "PIZARRA DE VUELOS AEROPUERTO INTERNACIONAL SIMON BOLIVAR\n\n" ++
                "Eliga una opción:"
    putStr $ "1) Ver pizarra\n" ++ "2) Cargar vuelos desde archivo\n" ++
                "3) Cargar vuelo manualmente\n" ++ "4) Eliminar vuelo\n"
                ++ "0) Salir\n" ++ "Su elección: "
    centro <- fmap (\(lE, lS) -> T.crearTablaVuelos lE lS) cargaArchivo
    putStrLn $ evalState (T.consultarVueloSalidaImp 18) centro
    putStrLn "Pizarra de vuelos de entrada" >>
                (mapM_ putStrLn $ evalState T.imprimirTablaEntrada centro) >>
                    putStrLn "Pizarra de vuelos de salida" >>
                        (mapM_ putStrLn $ evalState T.imprimirTablaSalida centro)
    return ()