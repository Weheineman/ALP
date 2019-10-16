module Main (main) where

import MyLexer
import MyParser

-- main = getContents >>= print . MyLexer.lexer
main = getContents >>= print . MyParser.parse . MyLexer.lexer
