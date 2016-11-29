module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar monadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip = return ()
evalComm (Let var i) = do i' <- evalIntExp i
                          update var i'
evalComm (Seq com1 com2) = do evalComm com1
                              evalComm com2
evalComm (Cond p com1 com2) = do p' <- evalBoolExp p
                                 case p' of
                                  True -> evalComm com1
                                  False -> evalComm com2
evalComm (While p com) = do p' <- evalBoolExp p
                            if p' then evalComm (Seq com (While p com)) else return ()
                            


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var v)   = lookfor v
evalIntExp (UMinus n)= do n' <- evalIntExp n
                          return (-n')
evalIntExp (Plus n1 n2) = do n <- evalIntExp n1
                             m <- evalIntExp n2
                             return (n+m)
evalIntExp (Minus n1 n2) = do n <- evalIntExp n1
                              m <- evalIntExp n2
                              return (n-m)
evalIntExp (Times n1 n2) = do n <- evalIntExp n1
                              m <- evalIntExp n2
                              return (n*m)
evalIntExp (Div n1 n2) = do n <- evalIntExp n1
                            m <- evalIntExp n2
                            return (n `div` m)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (Eq b1 b2) = do m <- evalIntExp b1
                            n <- evalIntExp b2
                            return (m == n)
evalBoolExp (Lt b1 b2) = do m <- evalIntExp b1
                            n <- evalIntExp b2
                            return (m < n)
evalBoolExp (Gt b1 b2) = do m <- evalIntExp b1
                            n <- evalIntExp b2
                            return (m > n)
evalBoolExp (And b1 b2) = do m <- evalBoolExp b1
                             n <- evalBoolExp b2
                             return (m && n)
evalBoolExp (Or b1 b2)  = do m <- evalBoolExp b1
                             n <- evalBoolExp b2
                             return (m || n)
evalBoolExp (Not b) = do b' <- evalBoolExp b
                         return (not b')