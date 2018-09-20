module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Error = DivByZero | UndefVar

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor _var [] = Left UndefVar
lookfor var ((v,i):xs)
	| v == var  = Right i
	| otherwise = lookfor var xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update var int [] 		= [(var, int)]
update var int ((v,i):xs)
	| v == var  = (var, int):xs
	| otherwise = update var int xs
	
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Either Error State
evalComm Skip state 		 = Right state
evalComm (Let var int) state   = 
	case (evalIntExp int state) of
		Left err  -> Left err
		Right val -> Right $ update var val state
evalComm (Seq com1 com2) state = 
	case evalComm com1 state of
		Left err -> Left err
		Right st -> evalComm com2 st
evalComm (Cond bool com1 com2) state = 
	case evalBoolExp bool state of
		Left err 	-> Left err
		Right True  -> evalComm com1 state
		Right False -> evalComm com2 state
evalComm rep@(Repeat com bool) state =
	case evalBoolExp bool state of
		Left err 	-> Left err
		Right True  -> Right state
		Right False ->
			case evalComm com state of
				Left err -> Left err
				Right st -> evalComm rep st
						
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntBinOp :: (Integer -> Integer -> Integer) -> Either Error Integer -> Either Error Integer -> Either Error Integer
evalIntBinOp _op (Left err) _int2 = Left err
evalIntBinOp _op _int1 (Left err) = Left err
evalIntBinOp op (Right int1) (Right int2) = Right $ op int1 int2

evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const int) _state = Right int
evalIntExp (Var var) state = lookfor var state
-- ~ evalIntExp (UMinus int) state = negate $ evalIntExp int state
evalIntExp (Plus int1 int2) state = evalIntBinOp (+) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Minus int1 int2) state = evalIntBinOp (-) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Times int1 int2) state = evalIntBinOp (*) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Div int1 int2) state = evalIntBinOp div (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Tern bool int1 int2) state =
	case evalBoolExp bool state of
		Left err    -> Left err
		Right True  -> evalIntExp int1 state
		Right False -> evalIntExp int2 state

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp _ _ = Right True
-- ~ evalBoolExp BTrue _state 		  = True
-- ~ evalBoolExp BFalse _state 		  = False
-- ~ evalBoolExp (Eq int1 int2) state    = (evalIntExp int1 state) == (evalIntExp int2 state)
-- ~ evalBoolExp (Lt int1 int2) state    = (evalIntExp int1 state) < (evalIntExp int2 state)
-- ~ evalBoolExp (Gt int1 int2) state    = (evalIntExp int1 state) > (evalIntExp int2 state)
-- ~ evalBoolExp (And bool1 bool2) state = (evalBoolExp bool1 state) && (evalBoolExp bool2 state)
-- ~ evalBoolExp (Or bool1 bool2) state  = (evalBoolExp bool1 state) || (evalBoolExp bool2 state)
-- ~ evalBoolExp (Not bool) state    	  = not (evalBoolExp bool state)
