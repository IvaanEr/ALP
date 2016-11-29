module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)  = Lam t (conversion' (n:b) u)
conversion' b (LLet x e t) = Let (conversion' b e) (conversion' (x:b) t)
conversion' b (LAs e t)    = As (conversion' b e) t
conversion' _ LUnit        = TUnit
conversion' b (LPair t1 t2) = TPair (conversion' b t1) (conversion' b t2)
conversion' b (LFst t)     = Fst (conversion' b t)
conversion' b (LSnd t)     = Snd (conversion' b t)
conversion' b (LSuc t)     = Suc (conversion' b t)
conversion' b (LZero)      = ZZero
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Let e u)             = Let (sub i t e) (sub (i+1) t u)
sub i t (As u t')             = sub i t u 
sub _ _ TUnit                 = TUnit
sub i t (TPair u v)           = TPair (sub i t u) (sub i t v)
sub i t (Fst u)               = Fst (sub i t u)
sub i t (Snd u)               = Snd (sub i t u)
sub i t (Suc u)               = Suc (sub i t u)
sub _ _ (ZZero)               = ZZero
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)  

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)       = case eval e v of
               VLam t' v' -> eval e (Lam t u :@: Lam t' v')
               VUnit      -> eval e (sub 0 TUnit u)
               VPair v1 v2-> eval e (sub 0 v u)
               Nv n       -> eval e (sub 0 v u)
               _          -> error "Error de tipo en run-time, verificar type checker -Lam"
eval e (Suc t :@: u)         = eval e (Suc (sub 0 u t))
eval e (u :@: v)             = case eval e u of
               VLam t u' -> eval e (Lam t u' :@: v)
               _         -> error "Error de tipo en run-time, verificar type checker-APP"
eval e (Let (Lam s v) t)     = eval e (sub 0 (Lam s v) t )
eval e (Let t u)             = case eval e t of
               VLam t' u' -> eval e (Let (Lam t' u') u)
               _          -> error "Error de tipo en run-time, verificar type checker-LET"
eval e (As u _)              = eval e u
eval _ TUnit                 = VUnit
eval e (TPair t1 t2)         = VPair (eval e t1) (eval e t2)
eval e (Fst t)               = case eval e t of
                VPair t' _ -> t'
                _          -> error "Error de tipo en run-time, verificar type checker -Fst"
eval e (Snd t)               = case eval e t of
                VPair _ t' -> t'
                _          -> error "Error de tipo en run-time, verificar type checker -Snd"
eval _ (ZZero)               = Nv Z
eval e (Suc t)               = case eval e t of
                Nv n -> Nv (Succ n)
                _    -> error "Error de tipo en run-time, verificar type checker -SUC"
eval e (Rec t1 t2 t3)        = case eval e t3 of
                (Nv Z)          -> eval e t1
                (Nv (Succ t3')) -> eval e ((t2 :@: (Rec t1 t2 (quote $ Nv t3'))):@: (quote $ Nv t3'))
                _               -> error "Error de tipo en run-time, verificar type checker -REC2"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote VUnit      = TUnit
quote (VPair t1 t2) = TPair (quote t1) (quote t2)
quote (Nv Z)        = ZZero
quote (Nv (Succ t)) = Suc (quote (Nv t))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

notPairError :: Type -> Either String Type
notPairError t1 = err $ "se esperaba Pair A B, pero" ++ render (printType t1) ++ "fue inferido."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                Nothing -> notfoundError n
                Just (_,t) -> ret t
infer' _ _ TUnit = ret Unit
infer' c e (t :@: u) = infer' c e t >>= \tt ->
                       infer' c e u >>= \tu ->
                       case tt of
                 Fun t1 t2 -> if (tu == t1)
                                then ret t2
                                else matchError t1 tu
                 _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (Let t u) = infer' c e t >>= \t' ->
                       infer' (t':c) e u
infer' c e (As t u)  = infer' c e t >>= \u' ->
                       if u == u' then ret u
                                  else matchError u u'
infer' c e (TPair t1 t2) = infer' c e t1 >>= \t1' ->
                           infer' (t1':c) e t2 >>= \t2' ->
                           ret (Pair t1' t2')
infer' c e (Fst t)       = infer' c e t >>= \t' ->
                            case t' of
                              Pair u _ -> ret u
                              _        -> notPairError t'
infer' c e (Snd t)       = infer' c e t >>= \t' ->
                            case t' of
                              Pair _ v -> ret v
                              _        -> notPairError t'
infer' c e (ZZero)       = ret Nat
infer' c e (Suc t)       = infer' c e t >>= \t' ->
                            case t' of
                              Nat -> ret Nat
                              _   -> matchError Nat t'
infer' c e (Rec t1 t2 t3) = infer' c e t1 >>= \t1' ->
                            infer' c e t2 >>= \t2' ->
                                if t2' == (Fun t1' (Fun Nat t1'))
                                then infer' c e t3 >>= \t3' ->
                                     case t3' of
                                       Nat -> ret t1'
                                       _   -> matchError Nat t3'
                                else matchError (Fun t1' (Fun Nat t1')) t2'