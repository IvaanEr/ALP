module Untyped where

--import Control.Monad
import Data.List
import Data.Maybe
import Common

----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

conversion  :: LamTerm -> Term
conversion lt = convert lt []

convert :: LamTerm -> [String] -> Term
convert (LVar st) xs = case findIndex (\x -> x == st) xs of
                          Nothing -> Free (Global st)
                          Just n -> Bound n
convert (App lt1 lt2) xs = (convert lt1 xs) :@: (convert lt2 xs)
convert (Abs st lt) xs   =  let st' = words st; ls = reverse st' ++ xs
                            in lams (length st') ls lt

lams :: Int -> [String] -> LamTerm -> Term
lams 0 ls lt = convert lt ls
lams n ls lt = Lam (lams (n-1) ls lt)

-------------------------------
-- Sección 3 - Evaluación
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) t2 = f t2
vapp (VNeutral (NFree n)) t2 = VNeutral (NApp (NFree n) t2)   
vapp (VNeutral (NApp n v)) t2 = VNeutral (NApp n (vapp v t2)) 
 
eval :: [(Name,Value)] -> Term -> Value
eval  e t = eval' t (e,[])

eval' :: Term -> (NameEnv Value,[Value]) -> Value
eval' (Lam t)      (d,d') = VLam (\x -> eval' t (d,x:d')) 
eval' (t1 :@: t2)  d      = vapp (eval' t1 d) (eval' t2 d)
eval' (Free n) d          = case find (\(x,_) -> x == n) (fst d) of
                             Nothing -> VNeutral (NFree n)
                             Just v  -> snd v
-- eval' (Free n)     d      = snd (fromJust (find (\(x,_) -> x == n) (fst d)))  
eval' (Bound  ii)  d      = (snd d) !! (ii)
 
--------------------------------
-- Sección 4 - Mostrando Valueores
--------------------------------

quote  :: Value -> Term
quote v = quote2 v 0

quote2 :: Value -> Int -> Term
quote2 (VLam f) i                       = Lam (quote2 (f (VNeutral (NFree (Quote i)))) (i+1))
quote2 (VNeutral (NFree (Global st))) _ = Free (Global st)
quote2 (VNeutral (NFree (Quote k))) i   = Bound (i-k-1)
quote2 (VNeutral (NApp neu v)) i        = (quote2 (VNeutral neu) i) :@: (quote2 v i)
