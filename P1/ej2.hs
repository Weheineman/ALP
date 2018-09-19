import Parsing
import Data.Char

data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Add | Mul | Min | Div deriving Show

expr :: Parser Expr
expr = do t <- term
          do{ char '+'
             ; e <- expr 
             ; return (BinOp Add t e)}
		  <|> 
		  do{ char '-'
            ; e1 <- expr 
            ; return (BinOp Min t e1) }
		   <|> return t         
		     
		      
term :: Parser Expr
term = do f <- factor
          (do{ char '*'
              ; t <- term 
              ; return (BinOp Mul f t)}
		        <|> 
                   do{ char '/'
                     ; t1 <- term 
                     ; return (BinOp Div f t1) }
                          <|> 
                             return f)         		      
		      
factor :: Parser Expr
factor = do{ d <- digit 
           ; return (Num (digitToInt d))}
         <|> do{ char '('
               ; e <- expr
               ; char ')'
               ; return e }   	      
                
eval xs =fst( head ( parse  expr xs))  

