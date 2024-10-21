{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []
  where
    conversion' vars (LVar name) = case elemIndex name vars of
                                  Nothing -> Free (Global name)
                                  Just i -> Bound i
    conversion' vars (LAbs name typee term) = let
                                                term' = conversion' (name:vars) term
                                              in
                                                Lam typee term'
    conversion' vars (LApp t1 t2) = let
                                      t1' = conversion' vars t1
                                      t2' = conversion' vars t2
                                    in
                                      t1' :@: t2'
    -- Ejercicio 4
    conversion' _ LZero = Zero
    conversion' vars (LSuc t) = Suc $ conversion' vars t
    conversion' vars (LRec t1 t2 t3) = let
                                        t1' = conversion' vars t1
                                        t2' = conversion' vars t2
                                        t3' = conversion' vars t3
                                       in
                                        Rec t1' t2' t3'
    -- Ejercicio 6
    conversion' _ LNil = Nil
    conversion' vars (LCons t1 t2) = let
                                      t1' = conversion' vars t1
                                      t2' = conversion' vars t2
                                     in
                                      Cons t1' t2'
----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
-- Construyo eval a partir de la funcion infer
eval :: NameEnv Value Type -> Term -> Value
eval nvs (Free n) = fst $ fromJust $ lookup n nvs -- infer previamente chequeo que la variable esta definida
eval nvs (Lam t u) = VLam t u
eval nvs (t1 :@: t2) = let
                        (VLam _ term1') = eval nvs t1
                        t2' = eval nvs t2
                        -- Elimina los bound
                        term1'' = sub 0 (quote t2') term1'
                       in
                        eval nvs term1''



----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu


