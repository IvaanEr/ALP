module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM
 
instance Applicative StateError where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance Monad StateError where
    return x = StateError (\s -> Just (x,s))
    m >>= f  = StateError (\s -> case runStateError m s of
                             Nothing -> Nothing
                             Just (v,s') -> runStateError (f v) s')

instance MonadError StateError where
    throw = StateError (\s-> Nothing)

instance MonadState StateError where
    lookfor v = StateError (\s -> case lookfor' v s of
                                   Nothing -> Nothing
                                   Just n  -> Just (n,s))  
                          where lookfor' v []          = Nothing
                                lookfor' v ((u, j):ss) | v == u = Just j
                                                       | v /= u = lookfor' v ss    
    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe Env
eval p = case runStateError (evalComm p) initState of
          Nothing -> Nothing
          Just (_,n) -> Just n

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
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
                            case m of 
                              0 -> throw
                              _ -> return (n `div` m)
 
-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
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
