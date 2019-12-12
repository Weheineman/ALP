module Common where

import           Token
import qualified Data.Set as Set


parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Variable Identifier
type Id = String

-- GUIDIOS: Hace falta TUnit?
-- Datatypes.
data Type
    = TUnit
    | TInt
    | TBool
    | TSet Type
    | TPair Type Type
    deriving (Show, Eq)

data Stm
    = CompoundStm Stm Stm
    | VarAssStm Type Id Exp
    | PrintStm Exp
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

-- Possible return values.
data RetValue
    = VInt Int
    | VBool Bool
    | VPair RetValue RetValue
    | VSet (Set.Set RetValue)
    | VType Type
    deriving (Show, Eq)

-- The return values have to be an instance of Ord in order to belong to a Set.
instance Ord RetValue where
  (VInt i1) `compare` (VInt i2) = i1 `compare` i2
  (VInt _) `compare` retVal = LT
  

-- Possible errors.
data Error
    = TypeError Type Type Exp
    | VarNotFound Id
    | VarExists Id

-- Pretty error printing.
instance Show Error where
  show (TypeError t1 t2 ex) =
    "\nExpected Type:"
      ++ show t1
      ++ "\nActual Type:"
      ++ show t2
      ++ "\nIn the expression:"
      ++ show ex
      ++ "\n"
  show (VarNotFound var) = "\nVariable " ++ var ++ " used but not declared.\n"
  show (VarExists   var) = "\nVariable " ++ var ++ " already declared.\n"
