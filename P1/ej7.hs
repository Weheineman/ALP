import Parsing
import Data.Char

expr :: Parser Int
expr = do{ t <- term
          ; e <- expr' 
          ; return (e t)}
		  
expr' :: Parser (Int -> Int)
expr' = do{ char '+'
             ; t <- term
             ; e <- expr' 
             ; return (\x-> e (x+t))}
		  <|> 
		  do{ char '-'
            ; t <- term
            ; e1 <- expr' 
            ; return (\x-> e1 (x-t)) }
		   <|> return id         
	   
		      
term :: Parser Int
term = do{ f <- factor
	 ; t <- term'
	 ; return (t f)}
--ACOMODAR!
term' :: Parser (Int -> Int)
term' = do{ char '*'
              ; f <- factor  
              ; t <- term' 
              ; return (\x -> t (f*x))}
		        <|> 
                   do{ char '/'
                     ; f <- factor                   
                     ; t1 <- term' 
                     ; return (\x-> t1 (div x f)) }
                          <|> 
                             return id         		      
		      
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
   						   	           


                         
