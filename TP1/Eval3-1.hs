module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]
data Op = O {sum :: Int, res :: Int, mul :: Int, div:: Int}

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado

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
eval :: Comm -> Maybe State
eval p = evalComm p (Just initState)

-- Evalua un comando en un estado dado
evalComm :: Comm -> Maybe (State,op) -> Maybe (State, Op)
evalComm _ Nothing = Nothing
evalComm Skip s = s
evalComm (Let x ie) (Just (s,o)) = case (evalIntExp ie (Just (s,o))) of
                        Nothing -> Nothing                       
                        Just (i,o1) -> Just ((update x i s),o1)
                        
evalComm (Seq c1 c2) t = case (evalComm c1 t) of
                        Nothing -> Nothing
                        t1 -> evalComm c2 t1  
                        
evalComm (Cond be c1 c2) (Just (s,o)) = case (evalBoolExp be s) of
                                Just (True,o1) -> evalComm c1 Just (s,o1)
                                Just (False,o2) -> evalComm c2 Just (s,o2)
                                Nothing -> Nothing
evalComm w@(While be c) (Just (s,o)) = case (evalBoolExp be s) of
                                Just (True,o1) -> evalComm (Seq c w) Just(s,o1)
                                Just (False,o2) -> evalComm c2 Just (s,o1)
                                Nothing -> Nothing
 

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> Maybe (State, Op) -> Maybe (Int,Op) 
evalIntExp _ Nothing = Nothing
evalIntExp (Const i) Just (s,op) = Just (i,op)
evalIntExp (Var x) (Just (s,op)) = Just ((lookfor x s),op)
evalIntExp (UMinus ie) t = evalU (evalIntExp ie t) ((-1)*)
evalIntExp (Plus ie1 ie2) t = evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (+) (1,0,0,0)
												 											
evalIntExp (Minus ie1 ie2) t = evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (-) (0,1,0,0)
evalIntExp (Times ie1 ie2) t = evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (*) (0,0,1,0)
evalIntExp (Div ie1 ie2) t= evalDiv (evalIntExp ie1 t) (evalIntExp ie2 t)

evalU :: Maybe (a,Op) -> (a -> a) -> Maybe (a,Op) 
evalU Nothing _ = Nothing
evalU (Just (a,op)) f = Just ((f a),op)

evalBin :: Maybe (a,Op) -> Maybe (a,Op) -> (a -> a -> b) -> Op->Maybe (b,Op) 
evalBin Nothing _ _ _= Nothing
evalBin _ Nothing _ _= Nothing
evalBin (Just (a,op1)) (Just (b,op2)) f op= Just ((f a b),sumar (sumar op1 op2) op) 

evalDiv :: Maybe Int -> Maybe Int -> Maybe Int
evalDiv Nothing _  = Nothing
evalDiv _ Nothing  = Nothing
evalDiv (Just a) (Just 0)  = Nothing
evalDiv (Just (a,op1)) (Just (b,op2)) = Just ((div a b), sumar (sumar op1 op2) (0,0,0,1)) 

sumar :: Op -> Op -> Op
sumar (a,b,c,d) (a1,b1,c1,d1) = (a+a1,b+b1,c+c1,d+d1)

-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: BoolExp -> Maybe (State,Op) -> Maybe (Bool,op)
evalBoolExp _ Nothing = Nothing
evalBoolExp BTrue Just(s,op)= Just (True,op)
evalBoolExp BFalse Just(s,op)= Just (False,op)
evalBoolExp (Eq ie1 ie2) t= evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (==) (0,0,0,0)
                              
evalBoolExp (Lt ie1 ie2) t= evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (<) (0,0,0,0)
evalBoolExp (Gt ie1 ie2) t= evalBin (evalIntExp ie1 t) (evalIntExp ie2 t) (>) (0,0,0,0)
evalBoolExp (And be1 be2) t= evalBin (evalBoolExp be1 t) (evalBoolExp be2 t) (&&) (0,0,0,0) 
evalBoolExp (Or be1 be2) t= evalBin (evalBoolExp be1 t) (evalBoolExp be2 t) (||) (0,0,0,0)
evalBoolExp (Not be) t= evalU (evalBoolExp be t) (not)
                              

