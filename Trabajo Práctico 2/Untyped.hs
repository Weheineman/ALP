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
shift = undefined
  
  
subst :: Term -> Term -> Int -> Term
subst = undefined    


eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
