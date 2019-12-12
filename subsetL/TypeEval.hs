module TypeEval where

import           Common
import           State

-- Checks that the two given types are equal. If they are not,
-- a TypeError is thrown.
checkEqualType :: (MonadState m, MonadError m) => Type -> Type -> Exp -> m ()
checkEqualType (TSet TUnit) (TSet t) ex = return ()
checkEqualType (TSet t) (TSet TUnit) ex = return ()
checkEqualType t1 t2 ex = if t1 /= t2 then throwType t1 t2 ex else return ()

typeCheck :: Stm -> Result Type
typeCheck p = runState (typeStm p) initEnv

-- Checks the type of a statement.
typeStm :: (MonadState m, MonadError m) => Stm -> m Type
typeStm (CompoundStm s1 s2) = do
  typeStm s1
  typeStm s2
typeStm (VarAssStm ty var ex) = do
  ty' <- typeExp ex
  checkEqualType ty ty' ex
  putValue var (VType ty)
  return ty
typeStm (PrintStm e) = do
  typeExp e
  return TUnit

-- Checks the type of an Expression List.
typeExpList :: (MonadState m, MonadError m) => ExpList -> m Type
typeExpList (SingleExp ex     ) = typeExp ex
typeExpList (ExpList ex exList) = do
  ty  <- typeExp ex
  ty' <- typeExpList exList
  checkEqualType ty' ty ex
  return ty

-- Checks the type of a binary operation, given the expected types of the
-- operands and the result.
typeBinOp
  :: (MonadState m, MonadError m)
  => Type
  -> Exp
  -> Type
  -> Exp
  -> Type
  -> m Type
typeBinOp type1 expr1 type2 expr2 retType = do
  type1' <- typeExp expr1
  checkEqualType type1 type1' expr1
  type2' <- typeExp expr2
  checkEqualType type2 type2' expr2
  return retType

-- Checks the type of a binary operation between sets (of the same type).
-- If the return type is TUnit, it returns the type of the sets.
typeSetBinOp :: (MonadState m, MonadError m) => Exp -> Exp -> Type -> m Type
typeSetBinOp expr1 expr2 retType = do
  TSet type1 <- typeExp expr1
  TSet type2 <- typeExp expr2
  checkEqualType (TSet type1) (TSet type2) expr2
  if retType == TUnit then return $ TSet type1 else return retType

-- Checks the type of an iterator list.
typeIterList :: (MonadState m, MonadError m) => IterList -> m ()
typeIterList (SingleIt var ex) = do
  TSet t <- typeExp ex
  putValue var (VType t)
  return ()
typeIterList (IterList var ex iterList) = do
  typeIterList (SingleIt var ex)
  typeIterList iterList

-- Frees all the variables in the IterList
cleanIterList :: (MonadState m, MonadError m) => IterList -> m ()
cleanIterList (SingleIt var ex) = do
  delEntry var
  return ()
cleanIterList (IterList var ex iterList) = do
  cleanIterList (SingleIt var ex)
  cleanIterList iterList

-- Checks the type of a Quantifier expression.
typeQuant :: (MonadState m, MonadError m) => IterList -> Exp -> m Type
typeQuant iterList ex = do
  typeIterList iterList
  t <- typeExp ex
  checkEqualType TBool t ex
  cleanIterList iterList
  return TBool

-- Checks the type of an expression.
typeExp :: (MonadState m, MonadError m) => Exp -> m Type
typeExp (Int  _  ) = return TInt
typeExp (Bool _  ) = return TBool
typeExp (Pair f s) = do
  tf <- typeExp f
  ts <- typeExp s
  return $ TPair tf ts
typeExp EmptySet    = return $ TSet TUnit
typeExp (SetExt el) = do
  tl <- typeExpList el
  return $ TSet tl
typeExp (SetComp iList ex) = do
  typeIterList iList
  t <- typeExp ex
  cleanIterList iList
  return $ TSet t
typeExp (Var var) = do
  VType t <- getValue var
  return t
typeExp (UnOp Minus ex) = do
  t <- typeExp ex
  checkEqualType TInt t ex
  return TInt
typeExp (UnOp First ex) = do
  TPair t1 _ <- typeExp ex
  return t1
typeExp (UnOp Second ex) = do
  TPair _ t2 <- typeExp ex
  return t2
typeExp (UnOp Card ex) = do
  TSet _ <- typeExp ex
  return TInt
typeExp (BinOp Add   ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TInt
typeExp (BinOp Sub   ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TInt
typeExp (BinOp Mul   ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TInt
typeExp (BinOp Div   ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TInt
typeExp (BinOp Mod   ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TInt
typeExp (BinOp Range ex1 ex2) = typeBinOp TInt ex1 TInt ex2 (TSet TInt)
typeExp (BinOp Lt    ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TBool
typeExp (BinOp Gt    ex1 ex2) = typeBinOp TInt ex1 TInt ex2 TBool
typeExp (BinOp Eq    ex1 ex2) = do
  t1 <- typeExp ex1
  t2 <- typeExp ex2
  checkEqualType t1 t2 ex2
  return TBool
typeExp (BinOp NEq  ex1 ex2) = typeExp (BinOp Eq ex1 ex2)
typeExp (BinOp And  ex1 ex2) = typeBinOp TBool ex1 TBool ex2 TBool
typeExp (BinOp Or   ex1 ex2) = typeBinOp TBool ex1 TBool ex2 TBool
typeExp (BinOp Elem ex1 ex2) = do
  t1 <- typeExp ex1
  t2 <- typeExp ex2
  checkEqualType (TSet t1) t2 ex2
  return TBool
typeExp (BinOp Subset      ex1 ex2) = typeSetBinOp ex1 ex2 TBool
typeExp (BinOp SubsetEq    ex1 ex2) = typeSetBinOp ex1 ex2 TBool
typeExp (BinOp Union       ex1 ex2) = typeSetBinOp ex1 ex2 TUnit
typeExp (BinOp Intersect   ex1 ex2) = typeSetBinOp ex1 ex2 TUnit
typeExp (BinOp Diff        ex1 ex2) = typeSetBinOp ex1 ex2 TUnit
typeExp (BinOp CartProduct ex1 ex2) = do
  TSet t1 <- typeExp ex1
  TSet t2 <- typeExp ex2
  return $ TSet (TPair t1 t2)
typeExp (Quant Exists iterList ex) = typeQuant iterList ex
typeExp (Quant ForAll iterList ex) = typeQuant iterList ex
