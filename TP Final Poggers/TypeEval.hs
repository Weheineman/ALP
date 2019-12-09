module TypeEval where

import Common
import State

checkEqualType :: (MonadState m, MonadError m) => Type -> Type -> Exp -> m a
checkEqualType t1 t2 ex = if t1 /= t2 then throwType t1 t2 ex

checkVarNotDeclared :: (MonadState m, MonadError m) => Id -> m a
-- GUIDIOS: Pensar por que esta bien que las funciones
-- de MonadState devuelvan valores monadicos.
checkVarNotDeclared var = if hasEntry var

-- typeCheck :: Stm -> Result a
-- typeCheck p = runState (typeStm p) initState

typeStm :: (MonadState m, MonadError m) => Stm -> m ()
typeStm (CompoundStm s1 s2) = do
    typeStm s1
    typeStm s2
typeStm (VarAssStm ty var ex) = do
    -- GUIDIOS: Agregar variable al entorno je
    ty' <- typeExp ex
    checkEqualType ty ty'
    return ty

typeExpList :: (MonadState m, MonadError m) => ExpList -> m Type
typeExpList (SingleExp ex) = typeExp ex
typeExpList (ExpList ex exList) = do
    ty  <- typeExp ex
    ty' <- typeExpList exList
    checkEqualType ty' ty ex
    return ty

typeExp :: (MonadState m, MonadError m) => Exp -> m Type
typeExp (Int _)  = return TInt
typeExp (Bool _) = return TBool
typeExp (Pair f s) = do
    tf <- typeExp f
    ts <- typeExp s
    return $ TPair tf ts
-- GUIDIOS: typeExp de EmptySet???
typeExp (SetExt el) = do
    tl <- typeExpList el
    return $ TSet tl
-- GUIDIOS: typeExp de SetComp
typeExp (Var Id) = do
