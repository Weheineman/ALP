-- Mónada estado
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

mapTreeNro :: (a->Int -> (b,Int)) -> Tree a -> Int -> (Tree b, Int)
mapTreeNro f (Leaf a) n = let (b,x) = f a n
				in (Leaf b, x)
mapTreeNro f (Branch l r) n = let (l',x) = mapTreeNro f l n
				in (let  (r', m) = mapTreeNro f r x
					in (Branch l' r', m))	
	  

numTree :: Tree a -> Tree Int
numTree t = fst (mapTreeNro update t 0)
		where update a n = (n,n+1)

probando = numTree (Branch (Branch (Leaf 'a') (Leaf 'b'))(Branch (Leaf 'b') (Leaf 'c')))

mapTreeM :: (a -> State s b ) -> Tree a -> State s (Tree b)
mapTreeM f (Leaf a) = State (\s -> let (h,s') = runState (f a) s
				in (Leaf h , s') )
{-
mapTreeM f (Branch l r) =  State (\s -> let (l',s') = runState (mapTreeM f l) s
					 in (let (r',s'') = runState(mapTreeM f r) s'
					     in ((Branch l' r'),s'') )	)
-}
mapTreeM f (Branch l r) = do		l' <- mapTreeM f l
					r' <- mapTreeM f r
					return (Branch l' r')

