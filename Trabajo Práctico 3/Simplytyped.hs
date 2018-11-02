module Simplytyped
  ( conversion -- conversion a terminos localmente sin nombre
  , eval -- evaluador
  , infer -- inferidor de tipos
  , quote -- valores -> terminos
  ) where

import           Common
import           Data.List
import           Data.Maybe
import           Prelude                   hiding ((>>=))
import           PrettyPrinter
import           Text.PrettyPrint.HughesPJ (render)

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u) = conversion' b t :@: conversion' b u
conversion' b (Abs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet x v e) = Let (conversion' b v) (conversion' (x : b) e)
conversion' b (LAs e t) = As (conversion' b e) t
conversion' b LUnit = Unit
conversion' b (LTup t1 t2) = Tup (conversion' b t1) $ conversion' b t2
conversion' b (LFst t) = Fst $ conversion' b t
conversion' b (LSnd t) = Snd $ conversion' b t
conversion' b LZero = Zero
conversion' b (LSucc t) = Succ $ conversion' b t
conversion' b (LRec t1 t2 t3) =
  Rec (conversion' b t1) (conversion' b t2) $ conversion' b t3

-----------------------
--- eval
-----------------------
sub :: Int -> Term -> Term -> Term
sub i t (Bound j)
  | i == j = t
sub _ _ (Bound j)
  | otherwise = Bound j
sub _ _ (Free n) = Free n
sub i t (u :@: v) = sub i t u :@: sub i t v
sub i t (Lam t' u) = Lam t' (sub (i + 1) t u)
sub i t (Let t' u) = Let (sub i t t') (sub (i + 1) t u)
sub i t (As t' ty) = As (sub i t t') ty
sub i t Unit = Unit
sub i t (Tup t1 t2) = Tup (sub i t t1) $ sub i t t2
sub i t (Fst t') = Fst (sub i t t')
sub i t (Snd t') = Snd (sub i t t')
sub i t Zero = Zero
sub i t (Succ t') = Succ (sub i t t')
sub i t (Rec t1 t2 t3) = Rec (sub i t t1) (sub i t t2) $ sub i t t3

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _) = error "variable ligada inesperada en eval"
eval e (Free n) = fst $ fromJust $ lookup n e
eval _ (Lam t u) = VLam t u
-- ~ eval e (Lam _ u :@: Lam s v)     = eval e (sub 0 (Lam s v) u)
-- ~ eval e (Lam _ u :@: Unit   )     = eval e (sub 0 Unit u)
-- ~ eval e (Lam _ u :@: (Tup t1 t2)) = eval e (sub 0 (quote $ eval e (Tup t1 t2)) u)
-- ~ eval e (Lam _ u :@: Zero)        = eval e (sub 0 Zero u)
-- ~ eval e (Lam _ u :@: (Succ t))    = eval e (sub 0 (quote $ eval e (Succ t)) u)
-- ~ eval e (Lam t u :@: v)           = eval e (Lam t u :@: (quote $ eval e v))
eval e (Lam t u :@: v) = eval e (sub 0 (quote $ eval e v) u)
eval e (u :@: v) =
  case eval e u of
    VLam t u' -> eval e (Lam t u' :@: v)
    _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t u) = eval e (sub 0 (quote $ eval e t) u)
eval e (As t ty) = eval e t
eval e Unit = VUnit
eval e (Tup t1 t2) = VTup (eval e t1) $ eval e t2
eval e (Fst t) =
  case eval e t of
    VTup v1 v2 -> v1
    _          -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd t) =
  case eval e t of
    VTup v1 v2 -> v2
    _          -> error "Error de tipo en run-time, verificar type checker"
eval e Zero = VNum NumZero
eval e (Succ t) =
  case eval e t of
    VNum v -> VNum $ NumSucc v
    _      -> error "Error de tipo en run-time, verificar type checker"
eval e (Rec t1 t2 t3) =
  case (eval e t3) of
    VNum NumZero -> eval e t1
    VNum (NumSucc v) -> eval e (t2 :@: (Rec t1 t2 t3') :@: t3')
      where t3' = quote $ VNum v
    _ -> error "Error de tipo en run-time, verificar type checker"

-----------------------
--- quoting
-----------------------
quote :: Value -> Term
quote (VLam t f)         = Lam t f
quote VUnit              = Unit
quote (VTup v1 v2)       = Tup (quote v1) $ quote v2
quote (VNum NumZero)     = Zero
quote (VNum (NumSucc v)) = Succ $ quote (VNum v)

----------------------
--- type checker
----------------------
-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) ::
     Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v

-- fcs. de error
matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err $
  "se esperaba " ++
  render (printType t1) ++
  ", pero " ++ render (printType t2) ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

nottupError :: Type -> Either String Type
nottupError t1 = err $ render (printType t1) ++ " no es una tupla."

notnatError :: Type -> Either String Type
notnatError t1 = err $ render (printType t1) ++ " no es un natural."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) =
  case lookup n e of
    Nothing     -> notfoundError n
    Just (_, t) -> ret t
infer' c e (t :@: u) =
  infer' c e t >>= \tt ->
    infer' c e u >>= \tu ->
      case tt of
        TypeFun t1 t2 ->
          if (tu == t1)
            then ret t2
            else matchError t1 tu
        _ -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ TypeFun t tu
infer' c e (Let t u) = infer' c e t >>= \t' -> infer' (t' : c) e u
infer' c e (As u t) =
  infer' c e u >>= \t' ->
    if (t' == t)
      then ret t
      else matchError t t'
infer' _ _ Unit = ret TypeUnit
infer' c e (Tup t1 t2) =
  infer' c e t1 >>= \t1' -> infer' c e t2 >>= \t2' -> ret $ TypeTup t1' t2'
infer' c e (Fst t) =
  infer' c e t >>= \t' ->
    case t' of
      TypeTup ty1 ty2 -> ret ty1
      _               -> nottupError t'
infer' c e (Snd t) =
  infer' c e t >>= \t' ->
    case t' of
      TypeTup ty1 ty2 -> ret ty2
      _               -> nottupError t'
infer' c e Zero = ret TypeNat
infer' c e (Succ t) =
  infer' c e t >>= \t' ->
    case t' of
      TypeNat -> ret t'
      _       -> notnatError t'
infer' c e (Rec t1 t2 t3) =
  infer' c e t1 >>= \t1' ->
    infer' c e t2 >>= \t2' ->
      infer' c e t3 >>= \t3' ->
        case t3' of
          TypeNat ->
            if t2' == TypeFun t1' (TypeFun TypeNat t1')
              then ret t1'
              else matchError (TypeFun t1' $ TypeFun TypeNat t1') t2'
          _ -> notnatError t3'
----------------------------------
