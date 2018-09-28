module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]


--Trazas
type Trace = [Comm]


-- Errores
data Error = DivByZero | UndefVar Variable deriving Show

-- Estado nulo
initState :: State
initState = []

-- Traza nula
initTrace :: Trace
initTrace = []


-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Integer
lookfor var [] = Left $ UndefVar var
lookfor var ((v,i):xs)
    | v == var  = Right i
    | otherwise = lookfor var xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update var int []         = [(var, int)]
update var int ((v,i):xs)
    | v == var  = (var, int):xs
    | otherwise = (v,i) : (update var int xs)    

-- Evalua un programa en el estado nulo
eval :: Comm -> (Either Error State, Trace)
eval p = case evalComm p initState initTrace of
             (Left err, tr) -> (Left err, reverse tr)
             (Right st,tr) -> (Right st, reverse tr)       

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Trace -> (Either Error State, Trace)
evalComm Skip state trace               = (Right state, trace)
evalComm (Let var int) state trace   = 
    case (evalIntExp int state) of
        Left err  -> (Left err, trace)
        Right val -> (Right  (update var val state), (Let var $ Const val) : trace)
evalComm (Seq com1 com2) state trace = 
    case evalComm com1 state trace of
        (Left err, tr) -> (Left err, trace)
        (Right st, tr) -> evalComm com2 st tr
evalComm (Cond bool com1 com2) state trace = 
    case evalBoolExp bool state of
        Left err    -> (Left err, trace)
        Right True  -> evalComm com1 state trace
        Right False -> evalComm com2 state trace
evalComm rep@(Repeat com bool) state trace = evalComm (Seq com (Cond bool Skip rep)) state trace 
                        
-- Evalua una expresion entera, sin efectos laterales
evalDiv :: Either Error Integer -> Either Error Integer -> Either Error Integer
evalDiv _e (Right 0) = Left DivByZero
evalDiv e1 e2 = (div) <$> e1 <*> e2

evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const int) _state = Right int
evalIntExp (Var var) state = lookfor var state
evalIntExp (UMinus int) state = negate <$> (evalIntExp int state)
evalIntExp (Plus int1 int2) state = (+) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalIntExp (Minus int1 int2) state = (-) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalIntExp (Times int1 int2) state = (*) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalIntExp (Div int1 int2) state = evalDiv (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Tern bool int1 int2) state =
	case evalBoolExp bool state of
		Left err    -> Left err
		Right True  -> evalIntExp int1 state
		Right False -> evalIntExp int2 state

-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue _state 		  = Right True
evalBoolExp BFalse _state 		  = Right False
evalBoolExp (Eq int1 int2) state    = (==) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalBoolExp (Lt int1 int2) state    = (<) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalBoolExp (Gt int1 int2) state    = (>) <$> (evalIntExp int1 state) <*> (evalIntExp int2 state)
evalBoolExp (And bool1 bool2) state = (&&) <$> (evalBoolExp bool1 state) <*> (evalBoolExp bool2 state)
evalBoolExp (Or bool1 bool2) state  = (||) <$> (evalBoolExp bool1 state) <*> (evalBoolExp bool2 state)
evalBoolExp (Not bool) state    	= not <$> (evalBoolExp bool state)
