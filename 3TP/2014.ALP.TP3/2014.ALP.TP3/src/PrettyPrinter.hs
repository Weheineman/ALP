module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos
--VER ORDEN DE REDUCCION!
pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  vs (Free (Global s)) = text s
pp ii vs (i :@: c) = sep [parensIf (isLam i || isLet i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c || isLet c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (Let t u) = text "let" <>
                     text (vs !! ii) <>
                     text "=" <>
                     pp ii vs t <>
                     text "in" <>
                     pp ii vs u 
pp ii vs (As t tt) = pp ii vs t <>
                     text "as" <>
                     printType tt
pp ii vs Unit      = text "unit"    
pp ii vs (Par t1 t2) = text "(" -- ACA no
                       pp ii vs t1
                       text ","
                       pp ii vs t2
                       text ")"
                                                             
                     
                                            
isFst (Fst _) = True
isFst _ = False

isSnd (Snd _) = True
isSnd _ = False
         
                     
isAs (As _ _ ) = True
isAs _         = False
                     
isLet (Let _ _) = True
isLet _           = False                     
                            
isLam (Lam _ _) = True
isLam  _      = False
   
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]

isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (Free _)          = []
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let x t u)       = fv t ++ fv u
  
---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

