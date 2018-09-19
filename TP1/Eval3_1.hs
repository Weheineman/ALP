module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

data Op = O {sum :: Int, res :: Int, mul :: Int, div:: Int}
-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
p :: Variable -> (Variable,Int) -> Bool
p x (y,_) = x==y

lookfor :: Variable -> State -> Int -- asumo que Variable aparece exactamente una vez en State
lookfor x xs = let [(x,i)] = filter (p x) xs
                in i

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update x i [] = [(x,i)]
update x i ((y,_):xs) = if x==y then (x,i):xs else update x i xs

-- Evalua un programa en el estado nulo
eval :: Comm -> (Maybe State, Op)
eval p = evalComm p (Just initState)

-- Evalua un comando en un estado dado
evalComm :: Comm -> Maybe State -> Op ->(Maybe State, Op) -- o que devuelva Maybe (State, Op)
evalComm _ Nothing o = (Nothing,o)
evalComm Skip s o= (s,o)
evalComm (Let x ie) (Just s) o = case (evalIntExp ie (Just s)) of
                        (Nothing,o1) -> (Nothing, sumar o o1)                       
                        (Just i,o1) -> (Just (update x i s), sumar o o1)
                        
evalComm (Seq c1 c2) s o= case (evalComm c1 s) of
                        (Nothing,o1) -> (Nothing, sumar o o1)
                        (s1,o1) -> evalComm c2 s1 (sumar o o1)  
                        
evalComm (Cond be c1 c2) s o= case (evalBoolExp be s) of
                                (Just True, o1) -> evalComm c1 s (sumar o o1)
                                (Just False, o1) -> evalComm c2 s (sumar o o1)
                                (_, o1) -> (Nothing,(sumar o o1))
                                
evalComm w@(While be c) s o= case (evalBoolExp be s) of
                                (Just True,o1) -> (evalComm (Seq c w) s,(sumar o o1)) -- asi o hacer directo??????
                                (Just False,o1) -> (s,sumar o o1)
                                (_,o1) -> (Nothing, (sumar o o1))
 

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> Maybe State -> (Maybe Int, Op) 
evalIntExp _ Nothing o = (Nothing, o)
evalIntExp (Const i) s o = (Just i,o) --PREG SI EL ESTADO ES NOTHING?
evalIntExp (Var x) (Just s) o = (Just (lookfor x s), o)
evalIntExp (UMinus ie) s o= evalU (evalIntExp ie s) ((-1)*) o
evalIntExp (Plus ie1 ie2) s o= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (+) o
evalIntExp (Minus ie1 ie2) s o= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (-) o
evalIntExp (Times ie1 ie2) s o= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (*) o
evalIntExp (Div ie1 ie2) s o = evalDiv (evalIntExp ie1 s) (evalIntExp ie2 s) o 
evalIntExp (Ter be ie1 ie2) s o= case (evalBoolExp be s) of
                                (Just True, o1) -> evalIntExp ie1 s (sumar o o1)
                                (Just False, o1) -> evalIntExp ie2 s (sumar o o1)
                                (_,o1) -> (Nothing, (sumar o o1))

evalU :: Maybe a -> (a -> a) -> Maybe a 
evalU Nothing _ = Nothing
evalU (Just a) f = Just (f a)

evalBin :: Maybe a -> Maybe a -> (a -> a -> b) -> Maybe b --PREG TIPO!
evalBin Nothing _ _ = Nothing
evalBin _ Nothing _ = Nothing
evalBin (Just a) (Just b) f = Just (f a b) 

evalDiv :: Maybe Int -> Maybe Int -> Maybe Int
evalDiv Nothing _  = Nothing
evalDiv _ Nothing  = Nothing
evalDiv (Just a) (Just 0)  = Nothing
evalDiv (Just a) (Just b) = Just (div a b) 

-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: BoolExp -> Maybe State -> Maybe Bool
evalBoolExp _ Nothing = Nothing
evalBoolExp BTrue s= Just True
evalBoolExp BFalse s= Just False
evalBoolExp (Eq ie1 ie2) s= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (==)
                              
evalBoolExp (Lt ie1 ie2) s= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (<)
evalBoolExp (Gt ie1 ie2) s= evalBin (evalIntExp ie1 s) (evalIntExp ie2 s) (>)
evalBoolExp (And be1 be2) s= evalBin (evalBoolExp be1 s) (evalBoolExp be2 s) (&&) 
evalBoolExp (Or be1 be2) s= evalBin (evalBoolExp be1 s) (evalBoolExp be2 s) (||)
evalBoolExp (Not be) s= evalU (evalBoolExp be s) (not)
                              

