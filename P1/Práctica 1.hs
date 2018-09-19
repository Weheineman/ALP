import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
-- Ejercicio 2
expr :: Parser Int
expr = do x <- term
          (plus x <|> minus x <|> empty x)
         where
           plus x = do char '+'
                       y <- expr
                       return (x+y)
           minus x = do char '-'
                        y <- expr
                        return (x-y)
           empty x = do string []
                        return x

term :: Parser Int
term = do x <- factor
          (mult x <|> divi x <|> empty x)
         where
           mult x = do char '*'
                       y <- expr
                       return (x*y)
           divi x = do char '/'
                       y <- expr
                       return (div x y)
           empty x = do string []
                        return x

factor :: Parser Int
factor = dig <|> paren
       where
         dig = do d <- digit
                  return (digitToInt d)
         paren = do char '('
                    e <- expr
                    char ')'
                    return e

-- Ejercicio 8
-- expr :: Parser Int
-- expr = do x <- term
          -- y <- expr'
          -- return x+y

-- expr' :: Parser Int
-- expr' = do string []
           -- return 0
         -- <|> do char '+'
                -- x <- term
                -- y <- expr'
                -- return x + y
         -- <|> do char '-'
                -- x <- term
                -- y <- expr'
                -- return -x + y
        
-- term :: 
