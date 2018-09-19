data Error er a = Raise er | Return a  deriving Show
instance Monad (Error er) where
    return x = Return x
    Raise er >>= f = Raise er
    Return a >>= f = f a 

head' :: [a] -> Error String a
head' [] = Raise "gwrg"
head' (x:xs) = return x


data T = Con Int | Div T T deriving Show

newtype M s e a = M {runM :: s -> Error e (a,s)} 

instance Monad (M s e) where
    return x = M (\s -> return (x, s))
    M m >>= f = M (\s -> case (m s) of
                    Raise e -> Raise e
                    Return (a,s') -> runM (f a) s')


eval :: T -> M Int String Int
eval (Con n) = return n
eval (Div t1 t2) = do v1 <- eval t1
                      v2 <- eval t2
                      if v2==0 then raise "Error"         
                             else do modify (+1)
                                     return (div v1 v2)

doEval t = runM (eval t) 0

raise :: String -> M Int String Int
raise s = M (\_ -> Raise s)

modify :: (Int -> Int) -> M Int String Int
modify f = M (\s -> return (10,f s))  
                          
