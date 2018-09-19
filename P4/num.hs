adivina :: Int -> IO ()
adivina n = do putStr "> "
               xs <- getLine
               m <- read xs
               if m == n then putStrLn "Esa es el numero!" else do putStrLn (cmp m n)
                                                                   adivina n
                       
cmp :: Int -> Int -> String
cmp m n = if m>n then "El numero ingresado es mayor!" else "El numero ingresado es menor!"                         

