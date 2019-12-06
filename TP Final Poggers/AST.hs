module AST where

import Token
import Common

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Stm
    = CompoundStm Stm Stm
    | VarAssStm Type Id Exp
    | PrintStm ExpList
    deriving Show

data ExpList
    = SingleExp Exp
    | ExpList Exp ExpList
    deriving Show

data Exp
    = Int Integer
    | Bool Bool
    | Pair Exp Exp
    | EmptySet
    | SetExt ExpList
    | SetComp IterList Exp
    | Var Id
    | UnOp UnOperator Exp
    | BinOp BinOperator Exp Exp
    | Quant Quantifier IterList Exp
    deriving Show

data UnOperator
    = First
    | Second
    | Card
    deriving Show

data BinOperator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Range
    | Lt
    | Gt
    | Eq
    | NEq
    | And
    | Or
    | Elem
    | Subset
    | SubsetEq
    | Union
    | Intersect
    | Diff
    deriving Show

data Quantifier
    = Exists
    | ForAll
    deriving Show

data IterList
    = SingleIt Id Exp
    | IterList Id Exp IterList
    deriving Show
