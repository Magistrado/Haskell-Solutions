{-
    Name: Daniel Leones
    Description: proyecto1 de sistemas de operación. Thread Experiment, stack transformers
    and getips for Haskell
    Date: 06/04/2016
    GHC Version: 7.10.3
    Ideas:
        - Data.Sequence to writers
        - Conditional execution of monadic expressions to do recursion
        - Add ExceptionT or standard error handling
        - Error control at parameters (try to use monadic manner)
        - Enable parameter handling to Version and verbose.

    Compile: ghc  -threaded  juegodedados_h.hs
    Execute: ./juegodedados_h +RTS -N4 -RTS -s 2003 -n 30 -j 20
-}

import Control.Monad.Trans.Writer.Lazy (Writer, tell, runWriter)
import Control.Monad.Trans.State.Lazy (StateT, get,evalStateT, runStateT, put)
import Control.Monad.Trans.Reader (ReaderT,runReaderT, ask)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (foldM )
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder (..), getOpt, usageInfo)
import System.Random (StdGen, randomR, mkStdGen)
import Control.Concurrent (MVar, forkIO, putMVar,takeMVar,
                           newEmptyMVar, getNumCapabilities)


mainJugador :: Int -> Int -> Int -> MVar Int -> IO ()
mainJugador sem idj num resultado = do
    let ((total, _), bit) = jugador_s sem num idj
    writeFile ("tiradas_" ++ (show idj) ++ ".txt") (unlines bit)
    putMVar resultado total


jugador_s sem num idj = runWriter $
    tell ["Player: " ++ (show idj)] >>
    tell ["Numero de tiradas: " ++ (show num)] >> runStateT (jugador') (0,0,mkStdGen sem)
    where
        jugador' :: StateT (Int,Int,StdGen) (Writer [String]) Int
        jugador' = do
            (cnt,total,nsem) <- get
            let (val, nsem') = randomR (1,12) nsem
            lift $ tell ["Tirada " ++ (show cnt) ++ ": " ++ (show val)]
            put (cnt + 1, total + val, nsem' )
            if num == cnt   then return $ total + val
                            else jugador'

crearJugadores :: ReaderT (Int, Int) (StateT (Int, Int, [(Int, MVar Int)]) IO) (Int,Int)
crearJugadores = do
    (maxNroJugadores, nroTiradas)  <- ask
    (cuenta , gen, listaResultado) <- lift get
    if cuenta <= maxNroJugadores then do
        resultado <- liftIO $ newEmptyMVar
        liftIO $ forkIO (mainJugador gen cuenta nroTiradas resultado)
        lift $ put (cuenta + 1, gen + 1, (cuenta,resultado):listaResultado)
        crearJugadores
    else foldM (\(jug,resul) (j,vj) -> do
                            x <- liftIO $ takeMVar vj
                            if x > resul
                                then return (j,x)
                                else return (jug,resul)) (0,0) listaResultado

croupier sem nroJug nroTiradas = do
    putStrLn $ "Numero de tiradas: " ++ show nroTiradas
    putStrLn $ "Semilla: " ++ show sem
    putStrLn $ "Jugadores: " ++ show nroJug
    (jug, resul) <- evalStateT (runReaderT crearJugadores (nroJug,nroTiradas)) (1,sem,[])
    putStrLn $ "**** Gana el jugador " ++ (show jug) ++ ", con " ++ (show resul) ++ " puntos"


data Flag = Verbose  | Version
            | Semilla Int | NroJug Int | NroTir Int
              deriving (Show)

options :: [OptDescr Flag]
options =
    [ Option ['h','?']     [] (NoArg Verbose)       "Imprime esta ayuda y sale"
    , Option ['s']     []  (ReqArg semilla "Entero")  "Semilla. (por defecto 1)"
    , Option ['n']     []  (ReqArg tiradas  "Entero")  "Tamaño de la secuencia. (por defecto, 10)"
    , Option ['j']     []  (ReqArg (NroJug  . read) "Entero") "Nro de jugadas"
    ]


semilla,tiradas :: String -> Flag
semilla = Semilla . read
tiradas = NroTir  . read

--compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]) -> croupier (semillas o) (juga o) (tir o)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where  header = "Uso: juegodedados_h []"
        semillas = foldl obtS (1)
        tir = foldl obtT (10)
        juga = foldl obtN (1)

        obtS acum (Semilla s) = s
        obtS acum _ =  acum

        obtN acum (NroJug n) = n
        obtN acum _ =  acum

        obtT acum (NroTir t) = t
        obtT acum _ =  acum

main = getNumCapabilities >>=  putStrLn . ("Thread number: "  ++) . show
                >> getArgs >>= compilerOpts

