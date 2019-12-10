module Main
  ( main
  )
where

import           MyLexer
import           MyParser
import           TypeEval

main = do
  file <- getContents
  print $ (typeCheck . MyParser.parse . MyLexer.lexer) file
