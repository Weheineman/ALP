module Eval where

import           Common
import           State

eval :: Stm -> Result Type
eval p = runState (evalStm p) initEnv

-- Evaluates a statement.
evalStm :: (MonadState m, MonadError m) => Stm -> m ()
evalStm (CompoundStm s1 s2) = do
  evalStm s1
  evalStm s2
evalStm (VarAssStm ty var ex) = do
  val <- evalExp ex
  putValue var val
  return ()
evalStm (PrintStm el) = do
-- GUIDIOS: Como printeamos aca???
  l <- evalExp
  return ()

-- Generates a VSet containing all the results of evaluating the
-- expressions in the ExpList.
-- GUIDIOS: Ver como hacer esto :)
-- evalExpList :: (MonadState m, MonadError m) => ExpList -> m RetValue
-- evalExpList (SingleExp ex     ) = evalExp ex
-- evalExpList (ExpList ex exList) = do
--   ty  <- evalExp ex
--   ty' <- evalExpList exList
--   checkEqualType ty' ty ex
--   return ty

-- Frees all the variables in the IterList
cleanIterList :: (MonadState m, MonadError m) => IterList -> m ()
cleanIterList (SingleIt var ex) = do
    delEntry var
    return ()
cleanIterList (IterList var ex iterList) = do
    cleanIterList (SingleIt var ex)
    cleanIterList iterList

-- Checks the type of an expression.
evalExp :: (MonadState m, MonadError m) => Exp -> m RetValue
evalExp (Int i) = return $ VInt i
evalExp (Bool b) = return $ VBool b
evalExp (Pair f s) = do
  vf <- evalExp f
  vs <- evalExp s
  return $ VPair vf vs
-- GUIDIOS: Uso esto pal emptySet?
evalExp EmptySet    = return $ VType TUnit
-- GUIDIOS: Hace falta meterle el VSet?
-- evalExp (SetExt el) = do
--   set <- evalExpList el
--   return $ VSet set
-- GUIDIOS: Re hard
-- evalExp (SetComp iList ex) = do
--   evalIterList iList
--   t <- evalExp ex
--   cleanIterList iList
--   return $ TSet t
evalExp (Var var) = do
  val <- getValue var
  return val
evalExp (UnOp First ex) = do
  VPair val _ <- evalExp ex
  return val
evalExp (UnOp Second ex) = do
  VPair _ val <- evalExp ex
  return val
-- GUIDIOS: Aprender a usar Set :)
-- evalExp (UnOp Card ex) = do
--   set <- evalExp ex
--   return Cardinalidad magica
evalExp (BinOp Add   ex1 ex2) = do
    i1 <- evalExp ex1
    i2 <- evalExp ex2
    return $ VInt (ex1 + ex2)
evalExp (BinOp Sub   ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TInt
evalExp (BinOp Mul   ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TInt
evalExp (BinOp Div   ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TInt
evalExp (BinOp Mod   ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TInt
evalExp (BinOp Range ex1 ex2) = evalBinOp TInt ex1 TInt ex2 (TSet TInt)
evalExp (BinOp Lt    ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TBool
evalExp (BinOp Gt    ex1 ex2) = evalBinOp TInt ex1 TInt ex2 TBool
evalExp (BinOp Eq    ex1 ex2) = do
  t1 <- evalExp ex1
  t2 <- evalExp ex2
  checkEqualType t1 t2 ex2
  return TBool
evalExp (BinOp NEq  ex1 ex2) = evalExp (BinOp Eq ex1 ex2)
evalExp (BinOp And  ex1 ex2) = evalBinOp TBool ex1 TBool ex2 TBool
evalExp (BinOp Or   ex1 ex2) = evalBinOp TBool ex1 TBool ex2 TBool
evalExp (BinOp Elem ex1 ex2) = do
  t1 <- evalExp ex1
  t2 <- evalExp ex2
  checkEqualType (TSet t1) t2 ex2
  return TBool
evalExp (BinOp Subset    ex1 ex2) = evalSetBinOp ex1 ex2 TBool
evalExp (BinOp SubsetEq  ex1 ex2) = evalSetBinOp ex1 ex2 TBool
evalExp (BinOp Union     ex1 ex2) = evalSetBinOp ex1 ex2 TUnit
evalExp (BinOp Intersect ex1 ex2) = evalSetBinOp ex1 ex2 TUnit
evalExp (BinOp Diff      ex1 ex2) = evalSetBinOp ex1 ex2 TUnit
evalExp (Quant Exists iterList ex) = evalQuant iterList ex
evalExp (Quant ForAll iterList ex) = evalQuant iterList ex
