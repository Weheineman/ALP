import Parsing
import Data.Char

expr :: Parser Int
expr = do t <- term
          do{ char '+'
             ; e <- expr 
             ; return (t+e)}
		  <|> 
		  do{ char '-'
            ; e1 <- expr 
            ; return (t-e1) }
		   <|> return t         
		     
		      
term :: Parser Int
term = do f <- factor
          (do{ char '*'
              ; t <- term 
              ; return (f*t)}
		        <|> 
                   do{ char '/'
                     ; t1 <- term 
                     ; return (div f t1) }
                          <|> 
                             return f)         		      
		      
factor :: Parser Int
factor = do{ d <- digit 
           ; return (digitToInt d)}
         <|> do{ char '('
               ; e <- expr
               ; char ')'
               ; return e }   	      
                
eval xs =fst( head ( parse (transformador expr) xs))     

transformador :: Parser a -> Parser a
transformador p = p <|> do{ char '('
   						  ; p1 <- p
   						  ; char ')'
   						  ; return p1 }
   						   	           


                         
