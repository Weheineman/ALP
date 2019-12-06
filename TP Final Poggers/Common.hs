module Common where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (ap, liftM)

-- Variable Identifier
type Id = String

-- GUIDIOS: Hace falta Unit?
-- Datatypes.
data Type
    = Unit
    | TInt
    | TBool
    | TSet Type
    | TPair Type Type
    deriving Show

-- Possible return values.
data RetValue
    = VInt Int
    | VBool Bool
    | VType Type

-- Environment.
type Env = [(Id, RetValue)]

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

-- Result of a (possibly failed) stateful computation.
data Result a
    = Value (a, Env)
    | Error String

-- State monad.
newtype State a = State { runState :: Env -> Result a }

instance Monad State where
    return x = State (\s -> Value (x, s))
    m >>= f = State $ \s ->
        case runState m s of
            Error msg     -> Error msg
            Value (v, s') -> runState (f v) s'

-- A Monad has to be an instance of Functor.
instance Functor State where
    fmap = liftM

-- A Monad has to be an instance of Applicative.
instance Applicative State where
    pure = return
    (<*>) = ap

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
