module TypeEval where

import Common
import State

checkEqualType :: (MonadState m, MonadError m) => Type -> Type -> Exp -> m ()
checkEqualType (TSet Unit) (TSet t) ex = return ()
checkEqualType (TSet t) (TSet Unit) ex = return ()
checkEqualType t1 t2 ex = if t1 /= t2 then throwType t1 t2 ex

-- typeCheck :: Stm -> Result a
-- typeCheck p = runState (typeStm p) initState

typeStm :: (MonadState m, MonadError m) => Stm -> m Type
typeStm (CompoundStm s1 s2) = do
    typeStm s1
    typeStm s2
typeStm (VarAssStm ty var ex) = do
    ty' <- typeExp ex
    checkEqualType ty ty'
    putValue var (VType ty)
    return ty
typeStm (PrintStm el) = do
    typeExp ex
    return Unit

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
typeExp EmptySet = return $ TSet Unit
typeExp (SetExt el) = do
    tl <- typeExpList el
    return $ TSet tl
-- GUIDIOS: typeExp de SetComp
typeExp (Var var) = do
    VType t <- getValue var
    return t
typeExp (UnOp First ex) = do
    TPair t1 t2 <- typeExp ex
    return t1
typeExp (UnOp Second ex) = do
    TPair t1 t2 <- typeExp ex
    return t2
typeExp (UnOp Card ex) = do
    TSet _ <- typeExp ex
    return TInt
