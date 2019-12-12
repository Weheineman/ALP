module Eval where

import           Common
import           State
import qualified Data.Set                      as Set


eval :: Stm -> Result ()
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
evalStm (PrintStm ex) = do
-- GUIDIOS: Como printeamos aca???
  evalExp ex
  return ()

-- Generates a list containing all the results of evaluating the
-- expressions in the ExpList.
evalExpList :: (MonadState m, MonadError m) => ExpList -> m [RetValue]
evalExpList (SingleExp ex) = do
  val <- evalExp ex
  return [val]
evalExpList (ExpList ex exList) = do
  val     <- evalExp ex
  valList <- evalExpList exList
  return (val : valList)

-- Frees all the variables in the IterList
cleanIterList :: (MonadState m, MonadError m) => IterList -> m ()
cleanIterList (SingleIt var ex) = do
  delEntry var
  return ()
cleanIterList (IterList var ex iterList) = do
  cleanIterList (SingleIt var ex)
  cleanIterList iterList

-- Evaluates an expression.
evalExp :: (MonadState m, MonadError m) => Exp -> m RetValue
evalExp (Int  i  ) = return $ VInt i
evalExp (Bool b  ) = return $ VBool b
evalExp (Pair f s) = do
  vf <- evalExp f
  vs <- evalExp s
  return $ VPair vf vs
evalExp EmptySet    = return $ VSet Set.empty
evalExp (SetExt el) = do
  valList <- evalExpList el
  return $ VSet (Set.fromList valList)
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
evalExp (UnOp Card ex) = do
  VSet set <- evalExp ex
  return VInt $ (smallInteger . Set.size) set
evalExp (BinOp Add ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  return $ VInt (i1 + i2)
evalExp (BinOp Sub ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  return $ VInt (i1 - i2)
evalExp (BinOp Mul ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  return $ VInt (i1 * i2)
evalExp (BinOp Div ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  if i2 == 0 then throwDivZero ex1 ex2 else return $ VInt (i1 `div` i2)
evalExp (BinOp Mod ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  if i2 == 0 then throwDivZero ex1 ex2 else return $ VInt (i1 `mod` i2)
evalExp (BinOp Range ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  if i1 < i2
    then throwRange ex1 i1 ex2 i2
    else return $ VSet ((Set.fromList . map VInt) [i1 .. i2])
evalExp (BinOp Lt ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  return $ VBool (ex1 < ex2)
evalExp (BinOp Gt ex1 ex2) = do
  VInt i1 <- evalExp ex1
  VInt i2 <- evalExp ex2
  return $ VBool (ex1 > ex2)
evalExp (BinOp Eq ex1 ex2) = do
  v1 <- evalExp ex1
  v2 <- evalExp ex2
  return $ VBool (ex1 == ex2)
evalExp (BinOp NEq ex1 ex2) = do
  v1 <- evalExp ex1
  v2 <- evalExp ex2
  return $ VBool (ex1 /= ex2)
evalExp (BinOp And ex1 ex2) = do
  VBool b1 <- evalExp ex1
  VBool b2 <- evalExp ex2
  return $ VBool (b1 && b2)
evalExp (BinOp Or ex1 ex2) = do
  VBool b1 <- evalExp ex1
  VBool b2 <- evalExp ex2
  return $ VBool (b1 || b2)
evalExp (BinOp Elem ex1 ex2) = do
  element  <- evalExp ex1
  VSet set <- evalExp ex2
  return VBool $ element `Set.member` set
evalExp (BinOp Subset ex1 ex2) = do
  VSet set1 <- evalExp ex1
  VSet set2 <- evalExp ex2
  return VBool $ set1 `Set.isProperSubsetOf` set2
evalExp (BinOp SubsetEq ex1 ex2) = do
  VSet set1 <- evalExp ex1
  VSet set2 <- evalExp ex2
  return VBool $ set1 `Set.isSubsetOf` set2
evalExp (BinOp Union ex1 ex2) = do
  VSet set1 <- evalExp ex1
  VSet set2 <- evalExp ex2
  return VSet $ set1 `Set.union` set2
evalExp (BinOp Intersect ex1 ex2) = do
  VSet set1 <- evalExp ex1
  VSet set2 <- evalExp ex2
  return VSet $ set1 `Set.intersection` set2
evalExp (BinOp Diff ex1 ex2) = do
  VSet set1 <- evalExp ex1
  VSet set2 <- evalExp ex2
  return VSet $ set1 `Set.difference` set2
-- GUIDIOS: Tmb hard y no quiero pensar ahora.
-- evalExp (Quant Exists iterList ex) = evalQuant iterList ex
-- evalExp (Quant ForAll iterList ex) = evalQuant iterList ex
