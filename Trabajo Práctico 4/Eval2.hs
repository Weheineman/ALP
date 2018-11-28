module Eval2
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
newtype StateError a = StateError
  { runStateError :: Env -> Maybe (a, Env)
  }

instance Monad StateError where
  return x = StateError (\s -> Just (x, s))
  m >>= f =
    StateError
      (\s ->
         case runStateError m s of
           Nothing      -> Nothing
           Just (v, s') -> runStateError (f v) s')

-- Clase para representar mónadas con estado de variables
class Monad m =>
      MonadState m
    -- Busca el valor de una variable
  where
  lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
  update :: Variable -> Int -> m ()

instance MonadState StateError where
  lookfor v = StateError (\s -> Just (lookfor' v s, s))
    where
      lookfor' v ((u, j):ss)
        | v == u = j
        | v /= u = lookfor' v ss
  update v i = StateError (\s -> Just ((), update' v i s))
    where
      update' v i [] = [(v, i)]
      update' v i ((u, _):ss)
        | v == u = (v, i) : ss
      update' v i ((u, j):ss)
        | v /= u = (u, j) : (update' v i ss)

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure = return
  (<*>) = ap

-- Clase para representar mónadas que lanzan errores
class Monad m =>
      MonadError m
    -- Lanza un error
  where
  throw :: m a

instance MonadError StateError where
  throw = StateError (\x -> Nothing)

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe Env
eval p = runStateError (evalComm p) initState >>= (Just . snd)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var v) = lookfor v
evalIntExp (UMinus i) = do
  n <- evalIntExp i
  return (-n)
evalIntExp (Minus i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  return (n1 - n2)
evalIntExp (Plus i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  return (n1 + n2)
evalIntExp (Times i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  return (n1 * n2)
evalIntExp (Div i1 i2) = do
  n1 <- evalIntExp i1
  n2 <- evalIntExp i2
  if n2 == 0
    then throw
    else return (n1 `div` n2)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
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
