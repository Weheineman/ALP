module AST where

import Token

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Variable Identifier
type Id = String

data Stm
    = CompoundStm Stm Stm
    | AssStm Id Exp
    | PrintStm ExpList
    deriving Show

data ExpList
    = EList Exp ExpList
    | Exp Exp
    deriving Show

data Exp
    = SetExp SetExp
    | IntExp IntExp
    deriving Show

data SetExp
    = EmptySet
    | ExpList ExpList
    | Brack SetExp
    deriving Show

data IntExp
    =
    | OperAdd IntExp IntExp
    | OperSub IntExp IntExp
    | OperMul IntExp IntExp
    | OperDiv IntExp IntExp
    deriving Show
