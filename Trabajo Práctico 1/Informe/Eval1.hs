module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Integer
lookfor var ((v,i):xs)
    | v == var  = i
	| otherwise = lookfor var xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update var int [] 		= [(var, int)]
update var int ((v,i):xs)
	| v == var  = (var, int):xs
	| otherwise = (v,i) : (update var int xs)
	
-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> State
evalComm Skip state		 	   = state
evalComm (Let var int) state   = update var (evalIntExp int state) state
evalComm (Seq com1 com2) state = evalComm com2 $ evalComm com1 state
evalComm (Cond bool com1 com2) state
	| evalBoolExp bool state   = evalComm com1 state
	| otherwise				   = evalComm com2 state
evalComm rep@(Repeat com bool) state = evalComm (Seq com (Cond bool Skip rep))state 

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const int) _state = int
evalIntExp (Var var) state = lookfor var state
evalIntExp (UMinus int) state = negate $ evalIntExp int state
evalIntExp (Plus int1 int2) state = (evalIntExp int1 state) + (evalIntExp int2 state)
evalIntExp (Minus int1 int2) state = (evalIntExp int1 state) - (evalIntExp int2 state)
evalIntExp (Times int1 int2) state = (evalIntExp int1 state) * (evalIntExp int2 state)
evalIntExp (Div int1 int2) state = (evalIntExp int1 state) `div` (evalIntExp int2 state)
evalIntExp (Tern bool int1 int2) state
	| evalBoolExp bool state = evalIntExp int1 state
	| otherwise				 = evalIntExp int2 state


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue _state 		  = True
evalBoolExp BFalse _state 		  = False
evalBoolExp (Eq int1 int2) state    = (evalIntExp int1 state) == (evalIntExp int2 state)
evalBoolExp (Lt int1 int2) state    = (evalIntExp int1 state) < (evalIntExp int2 state)
evalBoolExp (Gt int1 int2) state    = (evalIntExp int1 state) > (evalIntExp int2 state)
evalBoolExp (And bool1 bool2) state = (evalBoolExp bool1 state) && (evalBoolExp bool2 state)
evalBoolExp (Or bool1 bool2) state  = (evalBoolExp bool1 state) || (evalBoolExp bool2 state)
evalBoolExp (Not bool) state    	  = not (evalBoolExp bool state)


