import Parsing
import Data.Char

data Type = CInt | CChar | CFloat deriving Show
data Dr = P Dr | Arr Dr Int | Par Dr |I String deriving Show


type Ctype = (Type, Dr)

type_s :: Parser Type
type_s = do symbol "Int"
            return (CInt)
         <|> do symbol "Char"
                return (CChar)
             <|> do symbol "Float"
                    return (CFloat)
                    
declaration :: Parser Ctype
declaration = do t <- type_s
                 d <- declarator 
                 symbol ";"
                 return (t,d)
                 
direct_declarator :: Parser Dr
direct_declarator = do symbol "("
                       d <- direct_declarator
                       symbol ")"
                       f <- direct_declarator'
                       return (f (Par d))
                    <|> do s <- ident
                           f <- direct_declarator'
                           return (f (I s))
                     
direct_declarator' :: Parser (Dr -> Dr)
direct_declarator' = do symbol "["
                        e <- int
                        symbol "]"
                        f <- direct_declarator'
                        return (\x -> Arr (f x) e)
              <|>  return id                     
                                        
declarator :: Parser Dr
declarator = do symbol "*"
                d <- declarator
                return (P d) 
              <|> direct_declarator            
              
                                           
eval xs =fst( head ( parse declaration xs))                                           
                                                
