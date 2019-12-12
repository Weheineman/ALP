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
  let ast = (MyParser.parse . MyLexer.lexer) file in do
    print ast
    print $ typeCheck ast
    print $ eval ast
