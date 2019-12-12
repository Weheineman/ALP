module Main
  ( main
  )
where

import           MyLexer
import           MyParser
import           TypeEval
import           Eval

main = do
  file <- getContents
  print $ (typeCheck . MyParser.parse . MyLexer.lexer) file
  print $ (eval . MyParser.parse . MyLexer.lexer) file
