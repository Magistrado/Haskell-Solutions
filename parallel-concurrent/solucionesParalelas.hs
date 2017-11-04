{-
    Name: Daniel Leones
    Date: 4/11/2017
    Description:

    Soluciones a los problemas Super Computing and Distributed Systems
    Programming Contest Camp 2012

    Problem solutions to the Super Computing and Distributed Systems
    Programming Contest Camp 2012

    GHC Version: 7.10.3
    Compile: ghc -O2 -threaded solucioneParalelas.hs
    Execute: ./solucioneParalelas +RTS -N2 -RTS file.txt
-}

import Control.Parallel.Strategies (parMap, rdeepseq)
import System.Environment (getArgs)

-- Solution to problem A BOMBING FIELD
-- This solution use semiimplicit parallelism
exampleProbA = "10\n4\n0 0 8\n5 5 100\n1 1 -2\n7 7 -6\n5\n2 1 2 3\n1 1 1 4\n7 7 0 3\n6 6 4 8\n9 9 8 1\n"

bombing = stadistics . totalize . calcSeq . processInput
    where
        calcPar (lim, targets, bombs) =
            parMap rdeepseq (simulate lim bombs) targets
        calcSeq (lim, targets, bombs) =
            map (simulate lim bombs) targets
        simulate l bs [x,y,h] = foldl decisor (h,h) bs
            where
                decisor (h,h') [xb,yb,r,p]
                    | isLimit xb yb && reach xb yb r =
                        if h < 0 then (h,h' + p) else (h,h' - p)
                    | otherwise = (h,h')
                isLimit xb yb = 0 <= x && x < l && 0 <= y && y < l
                reach xb yb r =
                    xb - r <= x && x <= xb + r
                    && yb - r <= y && y <= yb + r

        totalize ls = foldl count (0,0,0,0,0,0) ls
            where
                count (mt,mp,mn,ct,cp,cn) (ho,hm)
                    | ho < 0 && ho == hm = (mt,mp,mn + 1,ct,cp,cn)
                    | ho < 0 && hm >= 0 = (mt + 1,mp,mn,ct,cp,cn)
                    | ho < 0 && ho < hm = (mt,mp + 1,mn,ct,cp,cn)
                    | ho > 0 && ho == hm = (mt,mp,mn,ct,cp,cn + 1)
                    | ho > 0 && hm <= 0 = (mt,mp,mn,ct + 1,cp,cn)
                    | otherwise = (mt,mp,mn,ct,cp + 1,cn)

        stadistics (mt,mp,mn,ct,cp,cn) = do
            putStrLn $ "Military targets totally destroyed: " ++ show mt
            putStrLn $ "Military targets partially destroyed: " ++ show mp
            putStrLn $ "Military targets not affected: " ++ show mn
            putStrLn $ "Civilian targets totally destroyed: " ++ show ct
            putStrLn $ "Civilian targets partially destroyed: " ++ show cp
            putStrLn $ "Civilian targets not affected: " ++ show cn

        processInput = bombs . targets . limtargets . lines
            where
                limtargets str =  (read $ head str, read $ head $ tail str, str)
                targets :: (Int, Int, [String]) -> (Int, [[Int]], Int, [String])
                targets (lim, cnttargets, str) =
                    (lim, (map (map read . words) . take cnttargets . drop 2) str
                        , cntBombs
                        , tail $ drop (2 + cnttargets) str
                    )
                    where
                        cntBombs = read $ head $ drop (2 + cnttargets) str
                bombs :: (Int, [[Int]], Int, [String]) -> (Int, [[Int]], [[Int]])
                bombs (lim, targets, cntBombs, str) =
                    (lim, targets, (map (map read . words) . take cntBombs) str)

main = getArgs >>= \[f] -> readFile f >>= bombing
