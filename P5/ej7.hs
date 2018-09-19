sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = x >>= (\x -> (sequence' xs >>= \s ->return (x:s)))
			
sequ :: Monad m => [m a] -> m [a]
sequ [] = return []
sequ (x:xs) = do m <-x
		 s <- sequ xs
		 return (m:s)
		 	
sequence'' :: Monad m => [m a] -> m [a]
sequence'' [m1,m2,m3]= m1 >>= \x1 ->
					m2 >>= \x2 ->
					m3 >>= \x3 ->					
					return [x1,x2, x3]

liftM :: Monad m => (a->b) -> m a -> m b
liftM f m = do x <- m
               return (f x)

liftM2 :: Monad m => (a->b->c) -> m a -> m b -> m c
liftM2 f m1 m2 =  (liftM (\x1 -> liftM (f x1) m2 ) m1) >>= id

liftM2' f m1 m2 = do x1<-m1
                     liftM (f x1) m2 

--sequen ::Monad m => [m a] -> m [a]
sequen l = foldr (liftM2 f) (return []) l 

f m l = m:l
