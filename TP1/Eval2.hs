module Eval2 (eval) where

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
lookfor v1 ((v2,int):xs) | v1 == v2  = int
                         | otherwise = lookfor v1 xs  

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v1 int1 [] = [(v1,int1)]
update v1 int1 ((v2,int2):xs) | v1 == v2  = ((v1,int1):xs)
                              | otherwise = ((v2,int2):(update v1 int1 xs)) 

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Maybe State
evalComm Skip xs                    = Just xs
evalComm (Let var x) xs             = if isNothing (evalIntExp x xs)
                                        then Nothing
                                        else Just (update var (fromJust (evalIntExp x xs)) xs)
evalComm (Seq comm1 comm2) xs       = if isNothing (evalComm comm1 xs)
                                        then Nothing
                                        else if isNothing (evalComm comm2 (fromJust (evalComm comm1 xs)))
                                             then Nothing
                                             else evalComm comm2 (fromJust (evalComm comm1 xs))
evalComm (Cond p comm1 comm2) xs    = case (evalBoolExp p xs) of
                                        Nothing -> Nothing
                                        (Just True) -> evalComm comm1 xs
                                        (Just False) -> evalComm comm2 xs
evalComm (While p comm) xs          = case (evalBoolExp p xs) of
                                        Nothing -> Nothing
                                        (Just True) -> (evalComm (Seq comm (While p comm)) xs)
                                        (Just False) -> Just xs



-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Maybe Int
evalIntExp (Const x) xs   = Just (fromInteger x)  
evalIntExp (Var v) xs     = Just (lookfor v xs)
evalIntExp (UMinus x) xs  = case (evalIntExp x xs) of
                             Nothing -> Nothing
                             (Just x) -> (Just (-x))
evalIntExp (Plus x y) xs  = aux xs (evalIntExp) x (+) y
evalIntExp (Minus x y) xs = aux xs (evalIntExp)  x (-) y
evalIntExp (Times x y) xs = aux xs (evalIntExp) x (*) y
evalIntExp (Div x y) xs   = case ((evalIntExp x xs), (evalIntExp y xs)) of
                              (Nothing,_) -> Nothing
                              (_,Nothing) -> Nothing
                              (a,b)       -> if (fromJust b) == 0 
                                             then Nothing 
                                             else (Just (div (fromJust a) (fromJust b)))
evalIntExp (AltCond p x y) xs = case (evalBoolExp p xs) of
                                        Nothing -> Nothing
                                        (Just True) -> evalIntExp x xs
                                        (Just False) -> evalIntExp y xs

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Maybe Bool
evalBoolExp BTrue xs     = Just True
evalBoolExp BFalse xs    = Just False
evalBoolExp (Eq x y) xs  = aux1 xs x (==) y
evalBoolExp (Lt x y) xs  = aux1 xs x (<) y
evalBoolExp (Gt x y) xs  = aux1 xs x (>) y
evalBoolExp (And x y) xs = aux xs (evalBoolExp) x (&&) y
evalBoolExp (Or x y) xs  = aux xs (evalBoolExp) x (||) y
evalBoolExp (Not x) xs   = let a = (evalBoolExp x xs)
                           in if isNothing a then Nothing
                                             else Just (not (fromJust a))

aux :: State -> (a -> State -> Maybe b) -> a -> (b -> b -> b) -> a -> Maybe b
aux xs e x op y = case ((e x xs),(e y xs)) of
                      (Nothing,_) -> Nothing
                      (_,Nothing) -> Nothing
                      (w,z)       -> Just (op (fromJust w) (fromJust z))
 
aux1 :: State -> IntExp -> (Int -> Int -> Bool) -> IntExp -> Maybe Bool
aux1 xs x op y = case ((evalIntExp x xs),(evalIntExp y xs)) of
                      (Nothing,_) -> Nothing
                      (_,Nothing) -> Nothing
                      (a,b)       -> Just (op (fromJust a) (fromJust b))

