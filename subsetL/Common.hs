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
    | TFun Type Type
    deriving (Eq, Ord)

instance Show Type where
  show TUnit         = "unit"
  show TInt          = "int"
  show TBool         = "bool"
  show (TSet t     ) = "set <" ++ show t ++ ">"
  show (TPair t1 t2) = "[" ++ show t1 ++ ", " ++ show t2 ++ "]"
  show (TFun  t1 t2) = show t1 ++ " -> " ++ show t2

data Stm
    = CompoundStm Stm Stm
    | VarAssStm Type Id Exp
    | FunDeclStm Type Id Type Id Exp
    | PrintStm Exp
    deriving Show

data ExpList
    = SingleExp Exp
    | ExpList Exp ExpList
    deriving (Show, Eq, Ord)

data Exp
    = Int Integer
    | Bool Bool
    | Pair Exp Exp
    | EmptySet
    | SetExt ExpList
    | SetComp IterList Exp
    | SetCompFilter IterList Exp Exp
    | Var Id
    | RetVal RetValue
    | FunApp Id Exp
    | UnOp UnOperator Exp
    | BinOp BinOperator Exp Exp
    | Quant Quantifier IterList Exp
    deriving (Show, Eq, Ord)

data UnOperator
    = Minus
    | First
    | Second
    | Card
    deriving (Show, Eq, Ord)

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
    deriving (Show, Eq, Ord)

data Quantifier
    = Exists
    | ForAll
    deriving (Show, Eq, Ord)

data IterList
    = SingleIt Id Exp
    | IterList Id Exp IterList
    deriving (Show, Eq, Ord)

-- Possible return values.
data RetValue
    = VInt Integer
    | VBool Bool
    | VPair RetValue RetValue
    | VSet (Set.Set RetValue)
    | VType Type
    | VFun Id Exp
    deriving (Eq, Ord)

-- Pretty value printing.
instance Show RetValue where
  show (VInt  i    ) = show i
  show (VBool b    ) = show b
  show (VType t    ) = show t
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  -- GUIDIOS: Esto esta feo
  show (VSet s     ) = show s
  show (VFun arg ex) = arg ++ " -> " ++ show ex

-- Possible errors.
data Error
    = TypeError Type Type Exp
    | VarNotFound Id
    | VarExists Id
    | DivZero Exp Exp
    -- | RangeErr Exp Integer Exp Integer

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
  -- show (RangeErr ex1 i1 ex2 i2) =
  --   "\nRange error. The first value should not be greater than the second.\nFirst expression: "
  --     ++ show ex1
  --     ++ " evaluates to "
  --     ++ show i1
  --     ++ "\nSecond expression: "
  --     ++ show ex2
  --     ++ " evaluates to "
  --     ++ show i2
  --     ++ "\n"


replaceVarInItList :: IterList -> Id -> RetValue -> IterList
replaceVarInItList (SingleIt var' ex) var retVal =
  SingleIt var' $ replaceVarInExp ex var retVal
replaceVarInItList (IterList var' ex iList) var retVal = IterList
  var'
  (replaceVarInExp ex var retVal)
  (replaceVarInItList iList var retVal)


replaceVarInExpList :: ExpList -> Id -> RetValue -> ExpList
replaceVarInExpList (SingleExp ex) var retVal =
  SingleExp $ replaceVarInExp ex var retVal
replaceVarInExpList (ExpList ex exList) var retVal = ExpList
  (replaceVarInExp ex var retVal)
  (replaceVarInExpList exList var retVal)


-- Replaces all ocurrences of the given variable in the expression by the given
-- return value.
replaceVarInExp :: Exp -> Id -> RetValue -> Exp
replaceVarInExp (Pair ex1 ex2) var retVal =
  Pair (replaceVarInExp ex1 var retVal) (replaceVarInExp ex2 var retVal)
replaceVarInExp (SetExt el) var retVal =
  SetExt $ replaceVarInExpList el var retVal
replaceVarInExp (SetComp il ex) var retVal =
  SetComp (replaceVarInItList il var retVal) (replaceVarInExp ex var retVal)
replaceVarInExp (SetCompFilter il boolEx ex) var retVal = SetCompFilter
  (replaceVarInItList il var retVal)
  (replaceVarInExp boolEx var retVal)
  (replaceVarInExp ex var retVal)
replaceVarInExp (Var var') var retVal =
  if var' == var then RetVal retVal else (Var var')
-- GUIDIOS: Funciones como argumento, aca iria algo de la pinta if funId == var then FunApp var
replaceVarInExp (FunApp funId ex) var retVal =
  FunApp funId $ replaceVarInExp ex var retVal
replaceVarInExp (UnOp op ex) var retVal =
  UnOp op $ replaceVarInExp ex var retVal
replaceVarInExp (BinOp op ex1 ex2) var retVal =
  BinOp op (replaceVarInExp ex1 var retVal) (replaceVarInExp ex2 var retVal)
replaceVarInExp (Quant q iList ex) var retVal =
  Quant q (replaceVarInItList iList var retVal) (replaceVarInExp ex var retVal)
replaceVarInExp ex _ _ = ex
