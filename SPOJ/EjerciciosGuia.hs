import Data.Char (toLower,isLetter)

{-
    http://www.spoj.com/problems/ADDREV
    Adding reversed numbers
-}
--ejem = "3\n24 1\n4358 754\n305 794"
main = getContents >>=  mapM_ print . conv
    where
        conv :: String -> [Int]
        conv = map ((\[n1,n2] -> revNum $ n1 + n2) . 
                    map (revNum . read)  . words) . tail . lines
        revNum = (fst . foldr acumular (0,1) . rev)
            where
                acumular d (acum, cont) = (acum + d*cont, cont* 10)
                -- Patron unfold
                rev 0 = []
                rev num =  r:rev d
                    where 
                        (d,r) = quotRem num 10

--ejem = "ello 3\nLove 1\nKitty 2\n."
mainHelloKit = getContents >>=  mapM_ putStr . input
    where
        -- input v1
        input = map (kitty . words) . lines
        -- input v2 
        --input2 = zipWith kitty . words
           -- where
        kitty :: [String] -> String
        kitty [w,n] = (unlines . take (length w) . 
                iterate rotate1 . concat . (replicate (read n))) w
        kitty _ = []
        -- Rota un caracter desde el principio hasta el final
        rotate1 :: String -> String
        rotate1 [] = ""
        rotate1 (x:xs) = rota1 x xs
            where
                rota1 a [] = [a]
                rota1 x (x':xs) = x': rota1 x xs  

listaImpar :: [a] -> ([a],[a])
listaImpar = (\(_,p,i) -> (p,i) ) . foldl (\(bool, basePa, baseIm) e -> 
                        if bool then (False,basePa,e:baseIm) 
                                else (True, e:basePa ,baseIm)) (False,[],[])

{-
    He is offside!
    www.spoj.pl/problems/OFFSIDE/
    Fecha de resolución: 17/12/2016 (Falla)

-}

--ejem = "2 3\n500 700\n700 500 500\n2 2\n200 400\n200 1000\n3 4\n530 510 490\n480 470 50 310\n0 0\n"

mainOffside = getContents >>=  mapM_ putStrLn . offside . lines
    where
        dosMins :: [Int] -> (Int,Int)
        dosMins [] = (-1,-1)
        dosMins [x,y] = (x,y)
        dosMins xs = foldl (\(min1,min2) e-> 
                                if min1 >= e then (e,min1) 
                                            else (min1,min2)) (10001,10001) xs
        offside :: [String] -> [String]
        offside [x] = []
        offside (_:a:d:is)
            | a1 < d2 = "Y" : offside is
            | d1 == a1 || (d1 == d2 && d2 == a1) = "N" : offside is
            | otherwise = "N" : offside is
            where 
                a1 = (minimum . map read . words) a 
                (d1,d2) = (dosMins . map read . words) d

{-
Candy I
http://www.spoj.com/problems/CANDY/
Fecha de resolución: 17/12/2016
-}

--ejem = "5\n1\n1\n1\n1\n6\n2\n3\n4\n-1"
mainCandy = getContents >>=  mapM_ (putStrLn . show) . candy . map read . words
    where        
        candy :: [Int] -> [Int]
        candy xs = ((\(_,results,_,_,_) -> reverse results) . foldl pross (head xs,[],0,0,[])) (tail xs)
            where
                pross (n,results,acum,cnt,packs) e
                    | cnt == n      = (e, resul:results,0,0,[])
                    | otherwise     = (n,results, acum + e , cnt + 1, e:packs)
                    where
                        resul = if (acum `mod` n) == 0
                                    then foldl (repartir (acum `div` n)) 0 packs
                                    else (-1)

                        repartir v b c = if (v - c) > 0 then b + (v - c) else b

{-
    Number Steps
    http://www.spoj.com/problems/NSTEPS/
    Fecha de resolución: 19/12/2016

-}

--ejem = "3\n4 2\n6 6\n3 4\n"

mainNSteps = getContents >>= mapM_ (putStrLn . steps 
                                    . map read . words) . tail . lines
    where
        steps :: [Int] -> String
        steps [x,y]
            | x == y || x == (y + 2) = if x `mod` 2 == 0 
                then show (x + y) else show (x + y - 1)
            | otherwise = "No Number"

{-
    Hangover
    http://www.spoj.com/problems/HANGOVER/
    Fecha de resolución: 19/12/2016
-}

--ejem =  "1.00\n3.71\n0.04\n5.19\n0.00\n"

mainHangover = getContents >>= mapM_ (putStrLn . hang . read ) . lines
    where
        hang :: Float -> String
        hang l 
            | l > 0.00 = hang' 2 0.0 
            | otherwise = ""
            where 
                hang' n acum =  if acum > l
                            then show  (round (n - 2)) ++ " card(s)" 
                            else hang' (n + 1) (acum + (1 / n))



{-
    Draw Mountains
    http://www.spoj.com/problems/DRAWM/
    Fecha de resolución: 19/12/2016 (Falla pese a que tiene los requisitos)
-}

{-ejem = "8\n1 2 3 2 3 3 2 1 0\n3\n1 0 0 1\n-1"
ejem1 = "14\n0 1 2 3 3 2 2 1 2 2 3 3 2 1 0\n-1"

ejemCalAltura1,ejemCalAltura2 :: [Int]
ejemCalAltura1 = [1, 2, 3, 2, 3, 3, 2, 1, 0]
ejemCalAltura2 = [1, 0, 0, 1]
ejemDibujar :: ([(String,Int,Int)],Int,Int)
ejemDibujar = ([("/",1,0),("/",2,1),("\\",2,2),("/",2,3),("_",3,4),("\\",2,5),("\\",1,6),("\\",0,7)],3,7)-}

mainDrawMountains = getContents >>= putStrLn . unlines . concat .  drawMountains . lines
   --where
drawMountains (_:hs:ls) = (dibujar . calAlturaPos  . map read . words) hs:["***"]:drawMountains ls
drawMountains _ = []

calAlturaPos :: [Int] -> ([(String,Int,Int)],Int)
calAlturaPos datos = 
    ( (\(acum,_,_,maxAl) -> (reverse acum,maxAl)) . 
       foldl empaquetar ([], 0, head datos, head datos) ) (tail datos)
    where
        {- 
            Probar: empaquetar ((_,alturaPrevia,pos):acum,a,cnt,hAnt,dif,maxAl) h
            Con esto se prodria ahorrar dos variables en la tuplax base.

            Probar con max y min , correspondientemente  
            para las alturas.
        -}
        empaquetar (acum,cnt,hAnt,maxAl) h
            | h - hAnt < 0 = (("\\", h, cnt):acum, cnt + 1, h, max h maxAl )
            | h - hAnt > 0 = (("/", hAnt , cnt):acum, cnt + 1, h, max h maxAl )
            | otherwise = (("_", hAnt ,cnt):acum,cnt + 1, h, h)

dibujar :: ([(String,Int,Int)],Int) -> [String]
dibujar (dat,maxAl) = (filter (""/=) . reverse) (particionar 0 dat)
    where 
        particionar a datos  = if maxAl < a then [] else
            (( concat . reverse  . fst . foldl cadena ([],0) . 
                filter (\(_,al,_) -> a == al)) datos): particionar (a + 1) datos 
        -- Patron unfold 
        cadena (capa,i) (ch,_,pos)
            | pos == i = (ch:capa, pos + 1)
            | pos > i  = ((replicate (pos - i) ' ' ++ ch):capa,pos + 1)


{-
    Triple Fat Ladies
    http://www.spoj.com/problems/EIGHTS/
    Fecha de resolución: 26/12/2016
-}

--ejem = "1\n1\n"

mainTriple8 = getContents >>= mapM_ (print . triple8 . read) . tail . lines
    where
        triple8 :: Integer -> Integer
        triple8 k = ((k-1) `div` 4)*1000 + 
            ([192,442,692,942] !! (fromInteger ((k-1) `mod` 4)))
        

{-
    Bullshit Bingo
    http://www.spoj.com/problems/BINGO/
    Fecha de resolución: 28/12/2016
-}

{-ejem = "Programming languages can be classified BULLSHIT into following types:\n- imperative and BULLSHIT procedural languages\n- functional languages\n- logical BULLSHIT programming languages\n- object-oriented BULLSHIT languages\n"-}


mainBingo = getContents >>= putStrLn . bingo . words . reemplazar
    where
        reemplazar :: String -> String
        reemplazar = foldr (\w acum -> 
                            if (not . isLetter) w
                                then ' ':acum 
                                else w:acum) []  

        bingo :: [String] -> String
        bingo = (\(pal,juegos,_) -> let (num,den) = reducirFrac pal juegos
                        in if den /= 1  then (show num) ++ " / " ++ (show den)
                                        else (show num)) . foldl jugar (0,0,[])
            where
                --jugar :: (Int,Int,[String]) -> String -> (Int,Int,[String])
                jugar (pal,juegos,acum) w = 
                    if w == "BULLSHIT"  
                        then (((pal +) . length . elimDup) acum ,juegos + 1,[])
                        else (pal,juegos, map toLower  w:acum)

                --elimDup :: [String] -> [String]
                elimDup ls = foldl (\new e -> if elem e new || e == ""    
                                then new 
                                else e:new) [] ls


        reducirFrac num den = 
            if gcd num den == 1 then (num,den) 
                                else reducirFrac (num `div` (gcd num den))
                                                  (den `div` (gcd num den))

{-
    Count on Cantor
    http://www.spoj.com/problems/CANTON/
    Fecha: 31/12/2016
-}
ejem = "3\n3\n14\n7\n"

mainCantor = getContents >>= mapM_ (print . cantor . read) . tail . words

cantor :: Integer -> String
cantor 1 = "TERM " ++ (show 1) ++ " IS " ++ (show 1) ++"/"++ (show 1)
cantor t = (\(num,den) -> "TERM " ++ (show t) 
                ++ " IS " ++ (show num) ++"/"++ (show den)) (cantorr 3 (2,3))
    where
        -- Idea: Usar intervalos cambiantes para evitar usar un contador
        cantorr c (ti,tf) 
            | ti <= t && t <= tf =  if (c - 1) `mod` 2 == 0 then (t - ti + 1, tf - t + 1) 
                                                            else (tf - t + 1,t - ti + 1)
            | otherwise = cantorr (c + 1) (tf + 1,tf + c)


