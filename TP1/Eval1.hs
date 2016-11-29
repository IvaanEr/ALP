module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Int
lookfor v1 ((v2,int):xs) | v1 == v2  = int
                         | otherwise = lookfor v1 xs  

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v1 int1 []                         = [(v1,int1)]
update v1 int1 ((v2,int2):xs) | v1 == v2  = ((v1,int1):xs)
                              | otherwise = ((v2,int2):(update v1 int1 xs)) 

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm (Skip) xs                  = xs
evalComm (Let var x) xs             = update var (evalIntExp x xs) xs
evalComm (Seq comm1 comm2) xs       = evalComm comm2 (evalComm comm1 xs)
evalComm (Cond p comm1 comm2) xs    = if (evalBoolExp p xs) then evalComm comm1 xs else evalComm comm2 xs
evalComm (While p comm) xs          = if (evalBoolExp p xs) then evalComm (Seq comm (While p comm)) xs else xs


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Int
evalIntExp (Const x) xs       = fromInteger x
evalIntExp (Var v) xs         = lookfor v xs
evalIntExp (UMinus x) xs      = - (evalIntExp x xs)
evalIntExp (Plus x y) xs      = (evalIntExp x xs) + (evalIntExp y xs)
evalIntExp (Minus x y) xs     = (evalIntExp x xs) - (evalIntExp y xs)
evalIntExp (Times x y) xs     = (evalIntExp x xs) * (evalIntExp y xs)
evalIntExp (Div x y) xs       = div (evalIntExp x xs) (evalIntExp y xs)
evalIntExp (AltCond p x y) xs = if (evalBoolExp p xs) then evalIntExp x xs else evalIntExp y xs

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue xs     = True
evalBoolExp BFalse xs    = False
evalBoolExp (Eq x y) xs  = (evalIntExp x xs) == (evalIntExp y xs)
evalBoolExp (Lt x y) xs  = (evalIntExp x xs) < (evalIntExp y xs)
evalBoolExp (Gt x y) xs  = (evalIntExp x xs) > (evalIntExp y xs)
evalBoolExp (And x y) xs = (evalBoolExp x xs) && (evalBoolExp y xs)
evalBoolExp (Or x y) xs  = (evalBoolExp x xs) || (evalBoolExp y xs)
evalBoolExp (Not x) xs   = not (evalBoolExp x xs)
