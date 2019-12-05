module Token where

data Token =
    TokenPrint       |
    TokenTInt        |
    TokenTBool       |
    TokenTSet        |
    TokenTrue        |
    TokenFalse       |
    TokenAnd         |
    TokenOr          |
    TokenSubset      |
    TokenSubsetEq    |
    TokenUnion       |
    TokenIntersect   |
    TokenDiff        |
    TokenIn          |
    TokenElem        |
    TokenFirst       |
    TokenSecond      |
    TokenExists      |
    TokenForAll      |
    TokenAss         |
    TokenDots        |
    TokenSemi        |
    TokenPipe        |
    TokenLBr         |
    TokenRBr         |
    TokenLCurlyBr    |
    TokenRCurlyBr    |
    TokenLParen      |
    TokenRParen      |
    TokenComma       |
    TokenPlus        |
    TokenMinus       |
    TokenTimes       |
    TokenDiv         |
    TokenMod         |
    TokenLt          |
    TokenGt          |
    TokenEq          |
    TokenNEq         |
    TokenCard        |
    TokenId String   |
    TokenInt Integer |
    TokenBool Bool
    deriving (Eq,Show)
