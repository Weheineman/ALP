module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Errores
data Error = DivByZero | UndefVar Variable deriving Show

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor var [] = Left $ UndefVar var
lookfor var ((v,i):xs)
	| v == var  = Right i
	| otherwise = lookfor var xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update var int [] 		= [(var, int)]
update var int ((v,i):xs)
	| v == var  = (var, int):xs
    | otherwise = (v,i) : (update var int xs)	

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
evalComm rep@(Repeat com bool) state = evalComm (Seq com (Cond bool Skip rep)) state 
						
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBinOp :: (a -> a -> b) -> Either Error a -> Either Error a -> Either Error b
evalBinOp _op (Left err) _e = Left err
evalBinOp _op _e (Left err) = Left err
evalBinOp op (Right e1) (Right e2) = Right $ op e1 e2

evalDiv :: Either Error Integer -> Either Error Integer -> Either Error Integer
evalDiv (Left err) _e = Left err
evalDiv _e (Left err) = Left err
evalDiv _e (Right 0) = Left DivByZero
evalDiv (Right e1) (Right e2) = Right $ div e1 e2

evalUniOp :: (a -> a) -> Either Error a -> Either Error a
evalUniOp _op (Left err) = Left err
evalUniOp op (Right e) = Right $ op e


evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const int) _state = Right int
evalIntExp (Var var) state = lookfor var state
evalIntExp (UMinus int) state = evalUniOp negate $ evalIntExp int state
evalIntExp (Plus int1 int2) state = evalBinOp (+) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Minus int1 int2) state = evalBinOp (-) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Times int1 int2) state = evalBinOp (*) (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Div int1 int2) state = evalDiv (evalIntExp int1 state) (evalIntExp int2 state)
evalIntExp (Tern bool int1 int2) state =
	case evalBoolExp bool state of
		Left err    -> Left err
		Right True  -> evalIntExp int1 state
		Right False -> evalIntExp int2 state

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue _state 		  = Right True
evalBoolExp BFalse _state 		  = Right False
evalBoolExp (Eq int1 int2) state    = evalBinOp (==) (evalIntExp int1 state) (evalIntExp int2 state)
evalBoolExp (Lt int1 int2) state    = evalBinOp (<) (evalIntExp int1 state) (evalIntExp int2 state)
evalBoolExp (Gt int1 int2) state    = evalBinOp (>) (evalIntExp int1 state) (evalIntExp int2 state)
evalBoolExp (And bool1 bool2) state = evalBinOp (&&) (evalBoolExp bool1 state) (evalBoolExp bool2 state)
evalBoolExp (Or bool1 bool2) state  = evalBinOp (||) (evalBoolExp bool1 state) (evalBoolExp bool2 state)
evalBoolExp (Not bool) state    	= evalUniOp not (evalBoolExp bool state)
