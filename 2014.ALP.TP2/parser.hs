import Parsing
import Data.Char
import Common
import Untyped

lam :: [String] -> LamTerm -> LamTerm
lam [x] t = Abs x t
lam (x:xs) t = Abs x (lam xs t)

app :: [LamTerm] -> LamTerm
app xs = app' (reverse xs)

app' :: [LamTerm] -> LamTerm
app' [x] = x
app' (x:xs) = App (app' xs) x

var_int :: Parser LamTerm
var_int = do
          i <- identifier
          return (LVar i)
         <|> do
            n <- natural
            return (num n)
             
lamb = do symbol "\\"--reservedOp untyped "\\"
          vars <- many1 identifier
          symbol "."--            reservedOp untyped "." 
          t <- parseLamTerm
          return (lam vars t)                                  
  
par = do symbol "("--reservedOp untyped "("
         t <- parseLamTerm
         symbol ")" --reservedOp untyped ")"
         return t 

          
-- Parser para LamTerms 
parseLamTerm :: Parser LamTerm
parseLamTerm = do 
                t <- many1 (var_int <|> lamb <|> par)                             
                return (app t)
            
evalp xs =fst( head ( parse (parseLamTerm) xs))            
