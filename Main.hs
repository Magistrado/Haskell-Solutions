module Main where

import CargaDatos(cargaArchivo, cargarVueloManual, TablaDestino(..))
import Control.Monad.State
import qualified TablaVuelos as T
import System.Exit(exitSuccess)

menu table = do
    putStrLn $ "\n\nPIZARRA DE VUELOS AEROPUERTO INTERNACIONAL SIMON BOLIVAR\n" ++
                "Eliga una opción:"
    putStr $ "1) Ver pizarra\n" ++ "2) Cargar vuelos desde archivo\n" ++
                "3) Cargar vuelo manualmente\n" ++ "4) Eliminar vuelo\n"
                ++ "0) Salir\n" ++ "Su elección: "
    elección <- getLine
    return ()
    -- Usar una excepcion para asegurar que sea un digito
    case (read elección :: Int) of
        1 -> printBlackboard table >> menu table
        2 -> do
                putStr "Introduza la ubicacion del archivo de vuelos:"
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
                        >>= (\tabla -> menu tabla)
        4 -> undefined
        otherwise -> exitSuccess


printBlackboard :: T.Tabla -> IO ()
printBlackboard table =
    putStrLn "\tPizarra de vuelos de entrada" >>
        (mapM_ putStrLn $ evalState T.imprimirTablaEntrada table) >>
            putStrLn "\tPizarra de vuelos de salida" >>
                (mapM_ putStrLn $ evalState T.imprimirTablaSalida table)



main :: IO ()
main = do
    --putStr "Introduza la ubicacion del archivo de vuelos:"
    --centro <- fmap (\(lE, lS) -> T.crearTablaVuelos lE lS) cargaArchivo
    menu T.crearTablaVuelosVacia

    --putStrLn $ evalState (T.consultarVueloSalidaImp 18) centro
    --printBlackboard centro
    return ()