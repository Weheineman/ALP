module Common where

import           Token
import qualified Data.Set                      as Set


parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Variable Identifier
type Id = String

-- GUIDIOS: Hace falta TUnit? Lo uso para el EmptySet
-- Datatypes.
data Type
    = TUnit
    | TInt
    | TBool
    | TSet Type
    | TPair Type Type
    deriving (Eq, Ord)

instance Show Type where
  show TUnit         = "unit"
  show TInt          = "int"
  show TBool         = "bool"
  show (TSet t     ) = "set <" ++ show t ++ ">"
  show (TPair t1 t2) = "[" ++ show t1 ++ ", " ++ show t2 ++ "]"

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
    = Minus
    | First
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
    | CartProduct
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
    = VInt Integer
    | VBool Bool
    | VPair RetValue RetValue
    | VSet (Set.Set RetValue)
    | VType Type
    deriving (Eq)

-- The return values have to be an instance of Ord in order to belong to a Set.
instance Ord RetValue where
  (VInt  i1   ) `compare` (VInt i2)     = i1 `compare` i2
  (VInt  _    ) `compare` retVal        = LT
  (VBool b1   ) `compare` (VBool b2)    = b1 `compare` b2
  (VBool _    ) `compare` retVal        = LT
  (VType t1   ) `compare` (VType t2)    = t1 `compare` t2
  (VType _    ) `compare` retVal        = LT
  (VPair v1 v2) `compare` (VPair v3 v4) = case v1 `compare` v3 of
    EQ  -> v2 `compare` v4
    ord -> ord
  (VPair _ _) `compare` retVal    = LT
  (VSet v1  ) `compare` (VSet v2) = v1 `compare` v2
  (VSet _   ) `compare` retVal    = LT

-- Pretty value printing.
instance Show RetValue where
  show (VInt  i    ) = show i
  show (VBool b    ) = show b
  show (VType t    ) = show t
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  -- GUIDIOS: Esto esta feo
  show (VSet s     ) = show s

-- Possible errors.
data Error
    = TypeError Type Type Exp
    | VarNotFound Id
    | VarExists Id
    | DivZero Exp Exp
    | RangeErr Exp Integer Exp Integer

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
  show (DivZero ex1 ex2) =
    "\nDivision by zero in expresssion\n"
      ++ show ex1
      ++ "\ndivided by\n"
      ++ show ex2
      ++ "\n"
  show (RangeErr ex1 i1 ex2 i2) =
    "\nRange error. The first value should be less than the second.\nFirst expression: "
      ++ show ex1
      ++ " evaluates to "
      ++ show i1
      ++ "\nSecond expression: "
      ++ show ex2
      ++ " evaluates to "
      ++ show i2
      ++ "\n"
