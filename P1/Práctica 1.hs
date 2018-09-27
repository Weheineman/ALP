import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)


-- ~ Ejercicio 2
-- ~ expr :: Parser Int
-- ~ expr = do x <- term
          -- ~ (plus x <|> minus x <|> empty x)
         -- ~ where
           -- ~ plus x = do char '+'
                       -- ~ y <- expr
                       -- ~ return (x+y)
           -- ~ minus x = do char '-'
                        -- ~ y <- expr
                        -- ~ return (x-y)
           -- ~ empty x = do string []
                        -- ~ return x

-- ~ term :: Parser Int
-- ~ term = do x <- factor
          -- ~ (mult x <|> divi x <|> empty x)
         -- ~ where
           -- ~ mult x = do char '*'
                       -- ~ y <- expr
                       -- ~ return (x*y)
           -- ~ divi x = do char '/'
                       -- ~ y <- expr
                       -- ~ return (div x y)
           -- ~ empty x = do string []
                        -- ~ return x

-- ~ factor :: Parser Int
-- ~ factor = integer <|> paren
       -- ~ where
         -- ~ paren = do char '('
                    -- ~ e <- expr
                    -- ~ char ')'
                    -- ~ return e

-- ~ Ejercicio 3

entreParen :: Parser a -> Parser a
entreParen p =  do char '('
                   ans <- entreParen p
                   char ')'
                   return ans
            <|> p
        

-- ~ Ejercicio 4

-- ~ data Expr = Num Int | BinOp Op Expr Expr deriving Show
-- ~ data Op = Add | Mul | Min | Div deriving Show

-- ~ expr4 :: Parser Expr
-- ~ expr4 = do t <- term4
           -- ~ (plus t <|> minus t <|> empty t)
         -- ~ where plus t  = do char '+'
                            -- ~ e <- expr4
                            -- ~ return $ BinOp Add t e
               -- ~ minus t = do char '-'
                            -- ~ e <- expr4
                            -- ~ return $ BinOp Min t e
               -- ~ empty t = do string []
                            -- ~ return t
               
-- ~ term4 :: Parser Expr
-- ~ term4 = do f <- factor4
           -- ~ (times f <|> div f <|> empty f)
         -- ~ where times f = do char '*'
                            -- ~ e <- term4
                            -- ~ return $ BinOp Mul f e
               -- ~ div   f = do char '/'
                            -- ~ e <- term4
                            -- ~ return $ BinOp Div f e
               -- ~ empty f = do string []
                            -- ~ return f

-- ~ factor4 :: Parser Expr
-- ~ factor4 = num <|> paren 
        -- ~ where num   = do n <- integer
                         -- ~ return $ Num n
              -- ~ paren = do char '('
                         -- ~ e <- expr4
                         -- ~ char ')'
                         -- ~ return e

-- ~ Ejercicio 5
-- ~ data IntOrChar = Num Int | Cha Char deriving Show

-- ~ intchar :: Parser IntOrChar
-- ~ intchar = num <|> cha
          -- ~ where num = do i <- integer
                         -- ~ return $ Num i
                -- ~ cha = do char '\''
                         -- ~ c <- item
                         -- ~ char '\''
                         -- ~ return $ Cha c

-- ~ intcharlist :: Parser [IntOrChar]
-- ~ intcharlist = do char '['
                 -- ~ xs <- sepBy intchar $ char ','
                 -- ~ char ']'
                 -- ~ return xs

-- ~ Ejercicio 6
-- ~ data Basetype = DInt | DChar | DFloat deriving Show
-- ~ type Hasktype = [Basetype]

-- ~ basetype :: Parser Basetype
-- ~ basetype = num <|> cha <|> flo
         -- ~ where num = (string "Int" >> return DInt)
               -- ~ cha = (string "Char" >> return DChar)
               -- ~ flo = (string "Float" >> return DFloat)

-- ~ hasktype :: Parser Hasktype
-- ~ hasktype = sepBy basetype $ string "->"

-- ~ Ejercicio 7

-- ~ data Hasktype = DInt | DChar | Fun Hasktype Hasktype deriving Show

-- ~ basetype :: Parser Hasktype
-- ~ basetype = num <|> cha
         -- ~ where num = (string "Int" >> return DInt)
               -- ~ cha = (string "Char" >> return DChar)
               
-- ~ hasktype :: Parser Hasktype
-- ~ hasktype = do t1 <- basetype
              -- ~ type2 t1 <|> empty t1
            -- ~ where empty t1 = (string [] >> return t1)
                  -- ~ type2 t1 = do string "->"
                                -- ~ t2 <- hasktype
                                -- ~ return $ Fun t1 t2

-- ~ Ejercicio 8
expr :: Parser Int
expr = do t <- term
          f <- expr'
          return $ f t
          
expr' :: Parser (Int -> Int)
expr' = plus <|> minus <|> empty
      where plus  = do char '+'
                       t <- term
                       f <- expr'
                       return $ f . (+t)
            minus = do char '-'
                       t <- term
                       f <- expr'
                       return $ f . (\x -> x-t) 
            empty = return id
          
term :: Parser Int
term = do n <- factor
          f <- term'
          return $ f n

term' :: Parser (Int -> Int)
term' = mult <|> divi <|> empty
     where mult  = do char '*'
                      n <- factor
                      f <- term'
                      return $ f . (*n)
           divi  = do char '/'
                      n <- factor
                      f <- term'
                      return $ f . (\ x -> div x n)
           empty = return id

factor :: Parser Int
factor = integer <|> entreParen expr

-- ~ Ejercicio 9
type ConstantExpression = Int
data TypeSpecifier = TInt | TChar | TFloat deriving Show
data DirectDeclarator = Id [Char]
                      | Arr DirectDeclarator ConstantExpression
                    deriving Show
data Declarator = Pointer Declarator | Dir DirectDeclarator deriving Show
data Declaration = Dec TypeSpecifier Declarator deriving Show

constant_expression :: Parser ConstantExpression
constant_expression = integer

type_specifier :: Parser TypeSpecifier
type_specifier = int <|> char <|> fl
               where int  = (token (string "int") >> return TInt)
                     char = (token (string "char") >> return TChar)
                     fl   = (token (string "float") >> return TFloat)

direct_declarator :: Parser DirectDeclarator
direct_declarator = do id <- token $ many alphanum 
                       f <- token direct_declarator'
                       return $ f (Id id)

direct_declarator' :: Parser (DirectDeclarator -> DirectDeclarator)
direct_declarator' = corch <|> empty
                   where corch = do char '['
                                    n <- token constant_expression
                                    char ']'
                                    f <- token direct_declarator'
                                    return $ f . (\ x -> Arr x n)
                         empty = return id

declarator :: Parser Declarator
declarator = point <|> direct
           where point  = do char '*'
                             d <- token declarator
                             return $ Pointer d
                 direct = do d <- token $ entreParen direct_declarator
                             return $ Dir d
                            
declaration :: Parser Declaration
declaration = do t <- token type_specifier
                 d <- token declarator
                 char ';'
                 return $ Dec t d
