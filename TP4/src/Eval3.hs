module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

--Creo el nuevo tipo StateErrorTick agregandole un tercer elemento a la tupla 
-- comparado con el eval anterior

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

-- a)
newtype StateErrorTick a = StateErrorTick {runStateErrorTick :: Env -> Maybe (a, Env, Int) }

instance Monad StateErrorTick where
  return x = StateErrorTick (\s -> Just (x, s, 0))
  m >>= f = StateErrorTick (\s -> case runStateErrorTick m s of
                                    Nothing -> Nothing
                                    Just (x, s', i) -> case runStateErrorTick (f x) s' of
                                                         Nothing -> Nothing
                                                         Just (y, s'', i') -> return (y,s'',i+i'))

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM
 
instance Applicative StateErrorTick where
    pure   = return
    (<*>)  = ap      

--b)
class Monad m => MonadTick m where
  tick :: m ()

--c)
instance MonadTick StateErrorTick where
  tick = StateErrorTick (\s -> Just ((),s ,1)) 

--d)
instance MonadError StateErrorTick where
  throw = StateErrorTick (\_ -> Nothing)

--e)
instance MonadState StateErrorTick where
  lookfor v = StateErrorTick (\s -> case lookfor' v s of
                                   Nothing -> Nothing
                                   Just n  -> Just (n,s,0))  
                          where lookfor' v []          = Nothing
                                lookfor' v ((u, j):ss) | v == u = Just j
                                                       | v /= u = lookfor' v ss 
  update v i = StateErrorTick (\s -> Just ((), update' v i s,0))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Evalua un programa en el estado nulo, devuelvo un par con el estado
-- y la cantidad de operaciones aritmeticas realizadas
eval :: Comm -> Maybe (Env, Int)
eval p = case runStateErrorTick (evalComm p) initState of
          Nothing -> Nothing
          Just (_,s,i) -> Just (s,i) 

-- Evalua un comando en un estado dado, idem eval 2 pero le agrego el MonadTick
-- En el tipo
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let var i) = do i' <- evalIntExp i
                          update var i'
evalComm (Seq com1 com2) = do evalComm com1
                              evalComm com2
evalComm (Cond b com1 com2) = do b' <- evalBoolExp b 
                                 case b' of 
                                  True -> evalComm com1
                                  _    -> evalComm com2

evalComm w@(While b com) = do b' <- evalBoolExp b
                              case b' of
                                True -> evalComm (Seq com w)
                                _    -> return ()

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const n) = return n 
evalIntExp (Var v)   = lookfor v
evalIntExp (UMinus n)= do n' <- evalIntExp n
                          return (-n')
evalIntExp (Plus n1 n2) = gralEvalTick n1 n2 (+) evalIntExp
evalIntExp (Minus n1 n2) = gralEvalTick n1 n2 (-) evalIntExp
evalIntExp (Times n1 n2) = gralEvalTick n1 n2 (*) evalIntExp
evalIntExp (Div n1 n2)   = do m <- evalIntExp n1
                              n <- evalIntExp n2
                              case m of 
                                0 -> throw
                                _ -> do tick
                                        return (m `div` n)

-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
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

-- Evaluador general para ahorarnos de escribir siempre el do
gralEvalTick :: (MonadState m, MonadError m, MonadTick m) => a -> a -> (b -> b -> b) -> (a -> m b) -> m b
gralEvalTick exp1 exp2 op eval = do exp1' <- eval exp1
                                    exp2' <- eval exp2
                                    tick
                                    return (op exp1' exp2')   
