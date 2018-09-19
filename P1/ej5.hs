import Parsing
import Data.Char

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype] 

p :: Parser Basetype
p = do string "Int"
       return DInt
     <|> do string "Char"
            return DChar
         <|> do string "Float"
                return DFloat
                
sep = symbol "->"                          

eval xs =fst( head ( parse (sepBy1 p sep) xs))     

