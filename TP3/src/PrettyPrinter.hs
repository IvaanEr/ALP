module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [1..], c <- ['x','y','z'] ++ ['a'..'w'] ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  vs (Free (Global s)) = text s
pp ii vs (i :@: c) = sep [parensIf (isLam i) (pp ii vs i),
                          nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <>
                     pp (ii+1) vs c
pp ii vs (Let e t) =   text "let" <>
                       text (vs !! ii) <>
                       text "=" <>
                       parensIf (isLam e || isApp e) (pp ii vs e) <>
                       text "in" <>
                       pp (ii+1) vs t
pp ii vs (As e t)  =   parensIf (isLam e || isApp e) (pp ii vs e) <>
                       text "as" <>
                       printType t
pp _ _ TUnit     = text "unit"
pp ii vs (TPair t1 t2) = text "(" <>
                         parensIf (isLam t1 || isApp t1) (pp ii vs t1) <>
                         text "," <>
                         parensIf (isLam t2 || isApp t2) (pp ii vs t2) <>
                         text ")"
pp ii vs (Fst t)       = text "fst" <+>
                         parensIf (isApp t || isLam t) (pp ii vs t)
pp ii vs (Snd t)       = text "snd" <+>
                         parensIf (isApp t || isLam t) (pp ii vs t)
pp ii vs (Rec t1 t2 t3) = text "R" <+>
                          parensIf (isApp t1 || isLam t1) (pp ii vs t1) <+>
                          parensIf (isApp t2 || isLam t2) (pp ii vs t2) <+>
                          parensIf (isApp t3 || isLam t3) (pp ii vs t3)
pp ii vs (Suc n)        = text "(" <>
                          text "suc" <+>
                          pp ii vs n <>
                          text ")"
pp ii vs (ZZero)        = text "Z"

isLam (Lam _ _) = True
isLam  _      = False

isApp (_ :@: _) = True
isApp _         = False

isPair (Pair _ _) = True
isPair _          = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType Unit         = text "Unit"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1),
                               text "->",
                               printType t2]
printType (Pair t1 t2) = text "(" <> --sep [ parensIf (isFun t1 || isPair t1) (printType t1),
                         printType t1 <>      -- text ",",
                         text "," <>      -- printType t2]
                         printType t2 <>
                         text ")"
printType Nat          = text "N"

isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (Free _)          = []
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let e t)         = fv e ++ fv t
fv (As e t)          = fv e
fv (TPair t1 t2)     = fv t1 ++ fv t2
fv (Fst t)           = fv t
fv (Snd t)           = fv t
fv (TUnit)           = []
fv (Suc t)           = fv t
fv (ZZero)           = []
fv (Rec t1 t2 t3)    = fv t1 ++ fv t2 ++ fv t3
  
---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t
