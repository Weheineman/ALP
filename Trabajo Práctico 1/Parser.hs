module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
-----------------------
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedOpNames = [":="]
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "while","do", "repeat", "until"] 
                                  })
  
  
  
----------------------------------
--- Operadores
-----------------------------------
iOperators = [ [Prefix (reservedOp lis "-" >> return (UMinus))          ]
             , [Infix  (reservedOp lis "*" >> return (Times))  AssocLeft,
                Infix  (reservedOp lis "/" >> return (Div))    AssocLeft]
             , [Infix  (reservedOp lis "+" >> return (Plus))   AssocLeft,
                Infix  (reservedOp lis "-" >> return (Minus))  AssocLeft]
             ]
             
bOperators = [ [Prefix (reservedOp lis "~" >> return (Not))          ]
             , [Infix  (reservedOp lis "&" >> return (And)) AssocLeft,
                Infix  (reservedOp lis "|" >> return (Or))  AssocLeft]
             ]

----------------------------------
--- Terminos
----------------------------------
iTerm :: Parser IntExp
iTerm =  try (parens lis intexp)
     <|> const
     <|> var
     <|> tern
  where const = do i <- integer lis
                   return $ Const i
        var   = do id <- identifier lis
                   return $ Var id
        tern  = do b <- boolexp
                   i1 <- intexp
                   i2 <- intexp
                   return $ Tern b i1 i2
         
bTerm :: Parser BoolExp
bTerm =  try (parens lis boolexp)
     <|> btrue
     <|> bfalse
     <|> relexp
  where btrue  = (reserved lis "true"  >> return BTrue)
        bfalse = (reserved lis "false" >> return BFalse)
        
----------------------------------
--- Parser de expresiones enteras
----------------------------------

intexp :: Parser IntExp
intexp = buildExpressionParser iOperators iTerm

-------------------------------------
--- Parser de expresiones de relacion
-------------------------------------
relexp :: Parser BoolExp
relexp = do i1 <- intexp
            op <- relOp
            i2 <- intexp
            return $ op i1 i2
          where relOp =  (reservedOp lis "=" >> return (Eq))
                     <|> (reservedOp lis "<" >> return (Lt))
                     <|> (reservedOp lis ">" >> return (Gt))
                  
-----------------------------------
--- Parser de expresiones booleanas
-----------------------------------

boolexp :: Parser BoolExp
boolexp =  buildExpressionParser bOperators bTerm

-----------------------------------
--- Parser de comandos
-----------------------------------

comm1 :: Parser Comm
comm1 =  skip 
     <|> ass
     <|> cond 
     <|> repeat
        where skip   = reserved lis "skip" >> return Skip
              ass    = do v <- identifier lis
                          reservedOp lis ":="
                          e <- intexp
                          return $ Let v e
              cond   = do reserved lis "if"
                          b <- boolexp
                          reserved lis "then"
                          c1 <- comm
                          reserved lis "else"
                          c2 <- comm
                          reserved lis "end"
                          return $ Cond b c1 c2
              repeat = do reserved lis "repeat"
                          c <- comm
                          reserved lis "until"
                          b <- boolexp
                          reserved lis "end"
                          return $ Repeat c b                



comm :: Parser Comm
comm = do list <- sepBy comm1 $semi lis;
          return $ seqC list
      where seqC [x]    = x
            seqC (x:xs) = Seq x $seqC xs
               

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
