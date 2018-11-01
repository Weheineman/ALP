module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = TypeBase 
            | TypeUnit
            | TypeNat
            | TypeFun Type Type
            | TypeTup Type Type
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LUnit
                |  LVar String
                |  LZero
                |  LSucc LamTerm
                |  LRec LamTerm LamTerm LamTerm
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  LLet String LamTerm LamTerm
                |  LAs LamTerm Type
                |  LTup LamTerm LamTerm
                |  LFst LamTerm
                |  LSnd LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Unit
             | Zero
             | Succ Term
             | Rec Term Term Term 
             | Term :@: Term
             | Lam Type Term
             | Let Term Term
             | As Term Type
             | Tup Term Term
             | Fst Term
             | Snd Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VUnit
             | VNum NumValue
             | VTup Value Value
             
  data NumValue = NumZero | NumSucc NumValue  

  -- Contextos del tipado
  type Context = [Type]
