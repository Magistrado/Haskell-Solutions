{-
    Module: Main
    Author: Daniel Leones
    Created: 21/12/12
    Purpose: This program is created for my practice in Haskell. I put myself to
    implement concept from Haskell. Also, practice functional programming
    Email: metaleones@gmail.com
    Please,
        - Send your proposals about improve my code skills,
            how to use better functional programming
-}

module Main where

import CargaDatos(cargaArchivo, cargarVueloManual, TablaDestino(..))
import Control.Monad.State
import qualified TablaVuelos as T
import System.Exit(exitSuccess)
import Text.Read (readMaybe)

menu table = do
    putStrLn $ "\n\nPIZARRA DE VUELOS AEROPUERTO INTERNACIONAL SIMON BOLIVAR\n" ++
                "Eliga una opción:"
    putStr $ "1) Ver pizarra\n" ++ "2) Cargar vuelos desde archivo\n" ++
                "3) Cargar vuelo manualmente\n" ++ "4) Eliminar vuelo\n"
                ++ "0) Salir\n" ++ "Su elección: "

    elección <- verifyInput
    case elección of
        1 -> printBlackboard table >> menu table
        2 -> do
                putStr "Introduza la ubicacion del archivo de vuelos:"
                -- Manejar errores de camino al archivo
                path <- getLine
                table' <- fmap (\(lE, lS) ->
                    execState (T.insertarListaVuelos lE lS) table
                    ) $ cargaArchivo path
                putStrLn "Carga exitosa"
                menu table'
        3 -> cargarVueloManual >>=
                (\(destino, vuelo) ->
                    case destino of
                        Salida -> undefined
                        Entrada -> return $
                                execState (T.insertarVueloEntrada vuelo) table )
                        >>= menu
        4 -> undefined
        otherwise -> exitSuccess


-- Implemented error checking
verifyInput :: IO Int
verifyInput = do
    elección <- getLine
    case readMaybe elección :: Maybe Int of
        Just x -> return x
        Nothing ->
            (putStrLn $ "ENTRADA INVALIDA." ++
                    "ESCRIBA UNA OPCION VALIDA, POR FAVOR") >>
            putStr "Su elección: " >> verifyInput

{-
    This function prints all the flights stored in T.Tabla. Departure and Arrivals.
-}

printBlackboard :: T.Tabla -> IO ()
printBlackboard table =
    putStrLn "\tPizarra de vuelos de entrada" >>
        (mapM_ putStrLn $ evalState T.imprimirTablaEntrada table) >>
            putStrLn "\tPizarra de vuelos de salida" >>
                (mapM_ putStrLn $ evalState T.imprimirTablaSalida table)



main :: IO ()
main = menu T.crearTablaVuelosVacia