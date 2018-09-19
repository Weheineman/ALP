{-findb:: String -> [(String,Int)] -> Maybe Int
findb s [] = Nothing
findb s ((x,i):xs) = if x==s then (Just i) else findb s xs

update :: String -> [(String,Int)] -> [(String,Int)]            --MEJORAR!!!                        
update s xs = case findb s xs' of
                   Nothing -> ((s,0):xs') 
                   Just i -> xs'
                   where xs' = (map (\(x,i) -> if x==s then (s,0) else (x,i+1) ) xs) 
-}  

