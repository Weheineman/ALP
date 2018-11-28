module Eval3
  ( eval
  ) where

import           AST
import           Control.Applicative (Applicative (..))
import           Control.Monad       (ap, liftM)

-- Estados
type Env = [(Variable, Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateErrorTick a = StateErrorTick
  { runStateErrorTick :: Env -> Maybe (a, Int, Env)
  }

instance Monad StateErrorTick where
  return x = StateErrorTick (\s -> Just (x, 0, s))
  m >>= f =
    StateErrorTick
      (\s ->
         case runStateErrorTick m s of
           Nothing -> Nothing
           Just (v1, i1, s1) ->
             case runStateErrorTick (f v1) s1 of
               Nothing           -> Nothing
               Just (v2, i2, s2) -> Just (v2, i1 + i2, s2))

-- Clase para representar mónadas con estado de variables
class Monad m =>
      MonadState m
    -- Busca el valor de una variable
  where
  lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
  update :: Variable -> Int -> m ()

instance MonadState StateErrorTick where
  lookfor v = StateErrorTick (\s -> Just (lookfor' v s, 0, s))
    where
      lookfor' v ((u, j):ss)
        | v == u = j
        | v /= u = lookfor' v ss
  update v i = StateErrorTick (\s -> Just ((), 0, update' v i s))
    where
      update' v i [] = [(v, i)]
      update' v i ((u, _):ss)
        | v == u = (v, i) : ss
      update' v i ((u, j):ss)
        | v /= u = (u, j) : (update' v i ss)

-- Para calmar al GHC
instance Functor StateErrorTick where
  fmap = liftM

instance Applicative StateErrorTick where
  pure = return
  (<*>) = ap

-- Clase para representar mónadas que lanzan errores
class Monad m =>
      MonadError m
    -- Lanza un error
  where
  throw :: m a

instance MonadError StateErrorTick where
  throw = StateErrorTick (\x -> Nothing)

class Monad m =>
      MonadTick m
          -- ~ Aumenta en 1 el contador
  where
  tick :: m ()

instance MonadTick StateErrorTick where
  tick = StateErrorTick (\s -> Just ((), 1, s))

-- Evalua un programa en el estado nulo
triSndAndThd :: (a, b, c) -> (b, c)
triSndAndThd (_, b, c) = (b, c)

eval :: Comm -> Maybe (Int, Env)
eval p = runStateErrorTick (evalComm p) initState >>= (Just . triSndAndThd)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let v i) = do
  m <- evalIntExp i
  update v m
evalComm (Seq c1 c2) = do
  evalComm c1
  evalComm c2
evalComm (Cond b c1 c2) = do
  m <- evalBoolExp b
  if m
    then evalComm c1
    else evalComm c2
evalComm w@(While b c) = evalComm $ Cond b (Seq c w) Skip

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var v) = lookfor v
evalIntExp (UMinus i) = do
  n <- evalIntExp i
  return (-n)
evalIntExp (Minus i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  tick
  return (n1 - n2)
evalIntExp (Plus i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  tick
  return (n1 + n2)
evalIntExp (Times i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  tick
  return (n1 * n2)
evalIntExp (Div i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  tick
  if n2 == 0
    then throw
    else return (n1 `div` n2)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (And b1 b2) = do
  m1 <- evalBoolExp b1
  m2 <- evalBoolExp b2
  return (m1 && m2)
evalBoolExp (Or b1 b2) = do
  m1 <- evalBoolExp b1
  m2 <- evalBoolExp b2
  return (m1 || m2)
evalBoolExp (Not b) = do
  m <- evalBoolExp b
  return (not m)
evalBoolExp (Eq i1 i2) = do
  m1 <- evalIntExp i1
  m2 <- evalIntExp i2
  return (m1 == m2)
evalBoolExp (Lt i1 i2) = do
  m1 <- evalIntExp i1
  m2 <- evalIntExp i2
  return (m1 < m2)
evalBoolExp (Gt i1 i2) = do
  m1 <- evalIntExp i1
  m2 <- evalIntExp i2
  return (m1 > m2)
