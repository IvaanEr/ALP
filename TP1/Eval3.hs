module Eval3 (eval) where

import AST
import Data.Maybe
-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Int
lookfor v1 ((v2,int):xs) | v1 == v2 = int
                         | otherwise = lookfor v1 xs  

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v1 int1 [] = [(v1,int1)]
update v1 int1 ((v2,int2):xs) | v1 == v2  = ((v1,int1):xs)
                              | otherwise = ((v2,int2):(update v1 int1 xs)) 

-- Evalua un programa en el estado nulo
eval :: Comm -> (Maybe State,Int)
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> (Maybe State,Int)
evalComm Skip xs                    = (Just xs,0)
evalComm (Let var x) xs             = let (a,b) = evalIntExp x xs
                                      in if isNothing a
                                         then (Nothing,b)
                                         else ((Just (update var (fromJust a) xs)),b)
evalComm (Seq comm1 comm2) xs       = let (a1,b1) = (evalComm comm1 xs)
                                          (a2,b2) = (evalComm comm2 (fromJust a1))
                                      in if isNothing a1
                                         then (Nothing,b1)
                                         else if isNothing a2
                                              then (Nothing,b1+b2)
                                              else (a2,b1+b2)
evalComm (Cond p comm1 comm2) xs    = case (evalBoolExp p xs) of
                                       (Nothing,i)      -> (Nothing,i)
                                       ((Just True),i)  -> let (a,b) = evalComm comm1 xs
                                                           in (a,b+i)
                                       ((Just False),i) -> let (a,b) = evalComm comm2 xs
                                                           in (a,b+i)
evalComm (While p comm) xs          = case (evalBoolExp p xs) of
                                       (Nothing,i)      -> (Nothing,i)
                                       ((Just True),i)  -> let (a,b) = (evalComm (Seq comm (While p comm)) xs)
                                                           in (a,b+i)
                                       ((Just False),i) -> (Just xs,i)



-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> (Maybe Int,Int)
evalIntExp (Const x) xs   = (Just (fromInteger x),0)  
evalIntExp (Var v) xs     = (Just (lookfor v xs),0)
evalIntExp (UMinus x) xs  = case (evalIntExp x xs) of
                             (Nothing,_)  -> (Nothing,0)
                             ((Just x),_) -> ((Just (-x)),0)
evalIntExp (Plus x y) xs  = aux xs x (+) y
evalIntExp (Minus x y) xs = aux xs x (-) y
evalIntExp (Times x y) xs = aux xs x (*) y
evalIntExp (Div x y) xs   = case ((evalIntExp x xs), (evalIntExp y xs)) of
                              ((Nothing,i),(_,j)) -> (Nothing,i+j+1)
                              ((_,i),(Nothing,j)) -> (Nothing,i+j+1)
                              ((a,i),(b,j))       -> if (fromJust b) == 0 
                                                     then (Nothing,i+j+1) 
                                                     else ((Just (div (fromJust a) (fromJust b))),i+j+1)
evalIntExp (AltCond p x y) xs = case (evalBoolExp p xs) of
                                       (Nothing,i)      -> (Nothing,i)
                                       ((Just True),i)  -> let (a,b) = evalIntExp x xs
                                                           in (a,b+i)
                                       ((Just False),i) -> let (a,b) = evalIntExp y xs
                                                           in (a,b+i)

aux :: State -> IntExp -> (Int -> Int -> Int) -> IntExp -> (Maybe Int,Int)
aux xs x op y = case ((evalIntExp x xs), (evalIntExp y xs)) of
                  ((Nothing,i),(_,j)) -> (Nothing,i+j+1)
                  ((_,i),(Nothing,j)) -> (Nothing,i+j+1)
                  ((a,i),(b,j))       -> ((Just (op (fromJust a) (fromJust b))),i+j+1)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> (Maybe Bool,Int)
evalBoolExp BTrue xs     = (Just True,0)
evalBoolExp BFalse xs    = (Just False,0)
evalBoolExp (Eq x y) xs  = aux1 xs x (==) y
evalBoolExp (Lt x y) xs  = aux1 xs x (<) y
evalBoolExp (Gt x y) xs  = aux1 xs x (>) y
evalBoolExp (And x y) xs = aux2 xs x (&&) y
evalBoolExp (Or x y) xs  = aux2 xs x (||) y
evalBoolExp (Not x) xs   = let (a,b) = (evalBoolExp x xs)
                           in if isNothing a then (Nothing,b)
                                             else ((Just (not (fromJust a))),b)

aux1 :: State -> IntExp -> (Int -> Int -> Bool) -> IntExp -> (Maybe Bool,Int)
aux1 xs x op y = case ((evalIntExp x xs),(evalIntExp y xs)) of
                      ((Nothing,i),(_,j)) -> (Nothing,i+j)
                      ((_,i),(Nothing,j)) -> (Nothing,i+j)
                      ((a,i),(b,j))       -> ((Just (op (fromJust a) (fromJust b))),i+j)

aux2 :: State -> BoolExp -> (Bool -> Bool -> Bool) -> BoolExp -> (Maybe Bool,Int)
aux2 xs x op y = case ((evalBoolExp x xs),(evalBoolExp y xs)) of
                      ((Nothing,i),(_,j)) -> (Nothing,i+j)
                      ((_,i),(Nothing,j)) -> (Nothing,i+j)
                      ((a,i),(b,j))       -> ((Just (op (fromJust a) (fromJust b))),i+j)