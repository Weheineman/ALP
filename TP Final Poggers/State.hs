module State where

import  Common
import  Control.Applicative (Applicative (..))
import  Control.Monad       (ap, liftM)

-- Environment.
type Env = [(Id, RetValue)]

-- Result of a (possibly failed) stateful computation.
data Result a
    = Value (a, Env)
    | Error Error

-- Initial state (empty environment).
initEnv :: Env
initEnv = []

-- Returns true iff there is an entry for the given variable.
hasEntry' :: Id -> Env -> Bool
hasEntry' var env = and $ map ((== var) . fst) env

-- Retrieves the value for the given variable in the environment.
getValue' :: Id -> Env -> RetValue
getValue' var ((var', val):xs)
    | var == var' = val
    | otherwise   = getValue' var xs

-- Stores the value for the given variable.
putValue' :: Id -> RetValue -> Env -> Env
putValue' var val env = (var, val):env

-- Removes the entry corresponding to the given variable.
delEntry' :: Id -> Env -> Env
delEntry' var ((var', val):xs)
    | var == var' = xs
    | otherwise   = (var', val):(delEntry' var xs)

-- State monad.
newtype State a = State { runState :: Env -> Result a }

instance Monad State where
    return x = State (\s -> Value (x, s))
    m >>= f = State $ \s ->
        case runState m s of
            Error err     -> Error err
            Value (v, s') -> runState (f v) s'

-- A Monad has to be an instance of Functor.
instance Functor State where
    fmap = liftM

-- A Monad has to be an instance of Applicative.
instance Applicative State where
    pure = return
    (<*>) = ap

-- A MonadError is a Monad that can throw errors.
class Monad m => MonadError m where
    throwType :: Type -> Type -> Exp -> m a
    throwVarNF :: Id -> m a

instance MonadError State where
    throwType t1 t2 ex = State(\s -> Error $ TypeError t1 t2 ex)
    throwType var = State(\s -> Error $ VarNotFound var)

-- A MonadState is a Monad with variable states.
class Monad m => MonadState m where
    -- Checks if the variable is bound in the current context.
    hasEntry :: Id -> m Bool
    -- Retrieves the value for the given variable.
    getValue :: Id -> m RetValue
    -- Stores the value for the given variable.
    putValue :: Id -> RetValue -> m ()
    -- Removes the entry corresponding to the given variable.
    delEntry :: Id -> m ()

-- Current instance where the state is a list of (id, RetValue) pairs.
instance MonadState State where
    hasEntry var = State (\s -> Value(hasEntry' var s, s))
    getValue var = State (\s -> Value(getValue' var s, s))
    putValue var val = State (\s -> Value((), putValue' var val s))
    delEntry var = State (\s -> Value((), delEntry' var s))
