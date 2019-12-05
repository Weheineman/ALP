module AST where

import Token

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Variable Identifier
type Id = String

data Stm
    = CompoundStm Stm Stm
    | VarAssStm Type Id Exp
    | PrintStm ExpList
    deriving Show

data ExpList
    = ExpList Exp ExpList
    | Exp Exp
    deriving Show

data Exp
    = Int Integer
    | Bool Bool
    | Pair Exp Exp
    | EmptySet
    | Set ExpList
    | Var Id
    | BinOp BinOperator Exp Exp
    | UnOp UnOperator Exp
    deriving Show

data BinOperator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Gt
    | Eq
    | NEq
    | And
    | Or
    | Subset
    | SubsetEq
    | In
    deriving Show

data UnOperator
    = Doot
    deriving Show

data Type
    = TInt
    | TBool
    | TSet Type
    | TPair Type Type
    deriving Show
