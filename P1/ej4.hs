import Parsing
import Data.Char


type Int_Char = Either Int Char

carac :: Parser Int_Char
carac = do { char ','
           ; char '\''
           ; l <-letter
	       ; char '\''
           ;return (Right l)}  

num :: Parser Int_Char
num = do { char ','
            ; d <- digit
            ; return (Left (digitToInt d))}

primero :: Parser Int_Char
primero = do { d <- digit
            ; return (Left (digitToInt d))}
             <|> 
           do{ ; char '\''
           ; l <-letter
	       ; char '\''
           ;return (Right l)}       
    
num_carac :: Parser [Int_Char]
num_carac = many (carac <|> num)

sep = symbol ","

list_nc :: Parser [Int_Char]
list_nc = do { char '['
             --; p <- primero
             ; p <- sepBy1 primero sep--nc <- num_carac 
             ; char ']'
             ; return (p{-:nc-})}
             
eval xs = fst( head ( parse list_nc xs))             
