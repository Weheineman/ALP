module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

findListPos :: Eq a => a -> [a] -> Maybe Int
findListPos x []      = Nothing
findListPos x (y:ys)
    | x==y      = Just 0
    | otherwise = (+1) <$> (findListPos x ys)

convAux :: LamTerm -> [Name] -> Term 
convAux (LVar s) l    =
    case findListPos s l of
        Nothing -> Free s
        Just n  -> Bound n
convAux (App t1 t2) l = (convAux t1 l) :@: (convAux t2 l)
convAux (Abs s t) l   = Lam $ convAux t (s:l)

conversion  :: LamTerm -> Term
conversion = (\x -> convAux x [])

  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift (Bound k) c d
    | k < c     = Bound k
    | otherwise = Bound $ k+d
shift (Free s) c d    = Free s
shift (t1 :@: t2) c d = (shift t1 c d) :@: (shift t2 c d)
shift (Lam t) c d     = Lam $ shift t (c+1) d
  
subst :: Term -> Term -> Int -> Term
subst (Bound k) t' i
    | k==i      = t'
    | otherwise = Bound k
subst (Free s) t' i    = Free s
subst (t1 :@: t2) t' i = (subst t1 t' i) :@: (subst t2 t' i)
subst (Lam t1) t' i     = Lam (subst t1 (shift t' 0 1) (i+1))

-- ~ COMO HACER CON LISTA VACIA????
freeEval :: NameEnv Term -> Name -> Term
freeEval [] name = Free name
freeEval ((name', t):xs) name
    | name == name' = t
    | otherwise     = freeEval xs name


eval :: NameEnv Term -> Term -> Term
eval env (Bound k)   = Bound k
eval env (Free s)    = freeEval env s
eval env (n@(Bound k) :@: t2) =  n :@: eval env t2
eval env (n@(Free s) :@: t2) = eval env (eval env n :@: t2)
eval env (Bound k) = Bound k


-- ~ eval :: NameEnv Term -> Term -> Term
-- ~ eval env (Free s) = lookInEnv s env
-- ~ eval env (Bound n) = Bound n
-- ~ eval env ((Free s) :@: lt2) = (Free s) :@: (eval env lt2)
-- ~ eval env ((Bound n) :@: lt2) = (Bound n) :@: (eval env lt2) 
-- ~ eval env ((Lam lt1) :@: lt2) = eval env (betaRed lt1 lt2)
-- ~ eval env (lt1 :@: lt2) = eval env ((eval env lt1) :@: lt2)
-- ~ eval env (Lam lt) = Lam (eval env lt)
