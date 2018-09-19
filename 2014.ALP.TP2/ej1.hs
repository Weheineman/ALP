data LamTerm = LVar String
			 | App LamTerm LamTerm
			 | Abs String LamTerm deriving Show
			 
num :: Integer -> LamTerm
num n = Abs "s" (Abs "z" (num' n)) 

num' :: Integer -> LamTerm
num' 0 = LVar "z"
num' (n+1) = App (LVar "s") (num' n)
