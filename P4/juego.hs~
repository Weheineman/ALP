
main :: IO ()
main = juego [5,4,3,2,1] True

juego :: [Int] -> Bool -> IO ()
juego t p = do jugador p
               xs <- getLine 
               putStr "Ingrese cuantas estrellas va a sacar " 
               ys <- getLine
               (let t' = (sacar t (read xs) (read ys))
                in if (isNil t') then (do putStrLn "Jugada incorrecta!"
                                          juego t p)
                             else (if (termino t') then putStr "Gano!"
                                                   else do  imprimir t' 
                                                            juego t' (not p)))
                    
               

jugador :: Bool -> IO ()
jugador p = if p then  putStr "1. Ingrese una fila " else putStr "2. Ingrese una fila " 

sacar :: [Int] -> Int -> Int -> [Int]
sacar t f n = let m = t!!(f-1)
              in if ((m >= n) ) then (act t (f-1) (m-n)) else []  

act :: [Int] -> Int -> Int -> [Int]
act [] _ _ = []
act (x:xs) 0 n = n:xs
act (x:xs) p n = x:(act xs (p-1) n) 

imprimir :: [Int] -> IO ()
imprimir [] = putStr "\n"
imprimir (x:xs) = if x>0 then (do putStr "* " 
                                  imprimir ((x-1):xs))
                          else (do putStr " \n"
                                   imprimir xs)            

isNil:: [Int] -> Bool
isNil [] = True
isNil _ = False                                    

termino :: [Int] -> Bool
termino [] = True
termino (0:xs) =(termino xs)
termino _ = False                                                                 
            
            
            
