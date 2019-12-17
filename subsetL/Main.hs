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
    case typeCheck ast of
      Error e -> print $ Error e
      doot -> do
        print doot
        print $ eval ast
