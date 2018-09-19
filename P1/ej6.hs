import Parsing
import Data.Char

data Hasktype = DInt | DChar | Fun Hasktype Hasktype deriving Show

p :: Parser Hasktype
p = do string "Int"
       return DInt
     <|> do string "Char"
            return DChar

fun :: Parser (Hasktype -> Hasktype)
fun = do symbol "->"
         p1 <- p
         f <- fun 
         return (\x -> Fun x (f p1))
      <|> return (\x ->x)   

hask :: Parser Hasktype
hask = do p1 <- p
          f <- fun
          return (f p1)                 
         
eval xs =fst( head ( parse (hask) xs))             
         
