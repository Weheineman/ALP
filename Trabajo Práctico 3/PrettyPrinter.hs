module PrettyPrinter
  ( printTerm -- pretty printer para terminos
  , printType -- pretty printer para tipos
  ) where

import           Common
import           Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos
pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k) = text (vs !! (ii - k - 1))
pp _ _ (Free (Global s)) = text s
pp ii vs (i :@: c) =
  sep
    [ parensIf (isNotAtom i) (pp ii vs i)
    , nest 1 (parensIf (isNotAtom c) (pp ii vs c))
    ]
pp ii vs (Lam t c) =
  text "\\" <> text (vs !! ii) <> text ":" <> printType t <> text ". " <>
  pp (ii + 1) vs c
pp ii vs (Let t c) =
  text "let " <> text (vs !! ii) <> text " = " <> pp ii vs t <> text " in " <>
  pp (ii + 1) vs c
pp ii vs (As c t) = pp ii vs c <> text " as " <> printType t
pp ii vs Unit = text "unit"
pp ii vs (Tup t1 t2) =
  text "(" <> pp ii vs t1 <> text ", " <> pp ii vs t2 <> text ")"
pp ii vs (Fst t) = text "fst " <> pp ii vs t
pp ii vs (Snd t) = text "snd " <> pp ii vs t
pp ii vs Zero = text "0"
pp ii vs (Succ t) = text "succ " <> pp ii vs t
pp ii vs (Rec t1 t2 t3) =
  sep
    [ text "R"
    , parensIf (isNotAtom t1) $ pp ii vs t1
    , parensIf (isNotAtom t2) $ pp ii vs t2
    , parensIf (isNotAtom t3) $ pp ii vs t3
    ]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isAtom :: Term -> Bool
isAtom (Free _)  = True
isAtom (Bound _) = True
isAtom Unit      = True
isAtom Zero      = True
isAtom (Tup _ _) = True
isAtom _         = False

isNotAtom :: Term -> Bool
isNotAtom = not . isAtom

-- pretty-printer de tipos
printType :: Type -> Doc
printType TypeBase = text "B"
printType TypeUnit = text "Unit"
printType TypeNat = text "Nat"
printType (TypeFun t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType (TypeTup t1 t2) =
  text "(" <> printType t1 <> text ", " <> printType t2 <> text ")"

isFun :: Type -> Bool
isFun (TypeFun _ _) = True
isFun _             = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv Unit              = []
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let t u)         = fv t ++ fv u
fv (As u t)          = fv u
fv (Tup t1 t2)       = fv t1 ++ fv t2
fv (Fst t)           = fv t
fv (Snd t)           = fv t
fv Zero              = []
fv (Succ t)          = fv t
fv (Rec t1 t2 t3)    = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t
