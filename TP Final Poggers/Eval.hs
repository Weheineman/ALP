module Eval
  ( eval
  )
where

import           AST
import           Control.Applicative            ( Applicative(..) )
import           Control.Monad                  ( ap
                                                , liftM
                                                )

-- Tipos
data MyType
    = Int Int
    | EmptySet
    | Set MyType
  deriving Show

-- Estados
type Env = [(Variable, MyType)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateError a = StateError
  { runStateError :: Env -> Maybe (a, Env)
  }

instance Monad StateError where
  return x = StateError (\s -> Just (x, s))
  m >>= f = StateError
    (\s -> case runStateError m s of
      Nothing      -> Nothing
      Just (v, s') -> runStateError (f v) s'
    )

-- Clase para representar mónadas con estado de variables
class Monad m =>
      MonadState m
    -- Busca el valor de una variable
  where
  lookfor :: Variable -> m MyType
    -- Cambia el valor de una variable
  update :: Variable -> MyType -> m ()

instance MonadState StateError where
  lookfor v = StateError (\s -> Just (lookfor' v s, s))
   where
    lookfor' v ((u, j) : ss) | v == u = j
                             | v /= u = lookfor' v ss
  update v i = StateError (\s -> Just ((), update' v i s))
   where
    update' v i []                     = [(v, i)]
    update' v i ((u, _) : ss) | v == u = (v, i) : ss
    update' v i ((u, j) : ss) | v /= u = (u, j) : (update' v i ss)

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
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
eval p = runStateError (evalStm p) initState >>= (Just . snd)

-- Evaluates a statement in a certain state
evalStm :: (MonadState m, MonadError m) => Stm -> m ()
evalStm (AssStm varName expr) = do
  value <- evalExp expr
  update varName value
evalStm (CompoundStm c1 c2) = do
  evalStm c1
  evalStm c2
evalStm (PrintStm expList) = do
  valList <- evalExpList expList
  map (putStr . show) valList
  return ()

evalExpList :: (MonadState m, MonadError m) => ExpList -> m [MyType]
evalExpList (Exp expr) = do
  return evalExp expr
evalExpList (EList expr expList) = do
  val     <- evalExp expr
  valList <- evalExpList expList
  return val : valList

evalExp :: (MonadState m, MonadError m) => Exp -> m MyType
evalExp (SetExp setExp) = evalSetExp setExp
evalExp (IntExp intExp) = evalIntExp intExp

evalIntExp :: (MonadState m, MonadError m) => IntExp -> m MyType
evalIntExp (OperAdd intExp term) = do
  expVal  <- evalIntExp intExp
  termVal <- evalTerm term
  return expVal + termVal
evalIntExp (OperSub intExp term) = do
  expVal  <- evalIntExp intExp
  termVal <- evalTerm term
  return expVal - termVal
evalIntExp ()


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue       = return True
evalBoolExp BFalse      = return False
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
