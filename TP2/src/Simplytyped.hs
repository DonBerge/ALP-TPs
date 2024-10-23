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
import           Prelude                 hiding ( (>>=), (>>) )
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
    -- Ejercicio 3
    conversion' vars (LLet name t1 t2) = let
                                          t1' = conversion' vars t1
                                          t2' = conversion' (name:vars) t2
                                         in
                                          Let t1' t2'
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
-- Ejercicio 3
sub i t (Let u v) = Let (sub i t u) (sub i t v)
-- Ejercicio 4
sub i t Zero = Zero
sub i t (Suc u) = Suc $ sub i t u
sub i t (Rec u v w) = let
                        u' = sub i t u
                        v' = sub i t v
                        w' = sub i t w
                      in
                        Rec u' v' w'

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f
-- Ejercicio 4
quote (VNum NZero) = Zero
quote (VNum (NSuc n)) = Suc $ quote $ VNum n
-- Ejercicio 6
quote (VList VNil) = Nil
quote (VList (VCons x xs)) = Cons (quote (VNum x)) (quote (VList xs))

-- evalúa un término en un entorno dado
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
-- Ejercicio 3
eval nvs (Let t u) = let
                      t' = eval nvs t
                      u' = sub 0 (quote t') u
                     in
                      eval nvs u'

-- Ejercicio 4
eval nvs Zero = VNum NZero
eval nvs (Suc n) = let 
                    (VNum n') = eval nvs n
                   in
                    VNum $ NSuc n'
eval nvs (Rec t1 t2 t3) = case eval nvs t3 of
                            VNum NZero -> eval nvs t1
                            VNum (NSuc t) -> let
                                                t' = quote $ VNum t
                                               in eval nvs $ t2 :@: Rec t1 t2 t' :@: t'
                            VList VNil -> eval nvs t1
                            VList (VCons n lv) -> let
                                                    n'  = quote $ VNum n
                                                    lv' = quote $ VList lv 
                                                  in
                                                    eval nvs $ t2 :@: n' :@: lv' :@: Rec t1 t2 lv'
-- Ejercicio 6
eval nvs Nil = VList VNil
eval nvs (Cons x xs) = let
                        (VNum x') = eval nvs x
                        (VList xs') = eval nvs x
                       in
                        VList $ VCons x' xs'

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

-- Funciones definidas por nosotros
match :: Type -> Either String Type -> Either String Type
match expected_type e@(Left _) = e
match expected_type e@(Right t)
  | expected_type == t = e
  | otherwise = matchError expected_type t

-- Funciones definidas por nosotros
checkIsFun :: Either String Type -> Either String Type
checkIsFun e@(Left _) = e
checkIsFun e@(Right t) = case t of
                          FunT t1 t2 -> e
                          _ -> notfunError t

(>>) :: Either String Type -> Either String Type -> Either String Type
(>>) v f = v >>= const f

matchMultiple :: [(Type, Either String Type)] -> Either String Type -> Either String Type
matchMultiple _ e@(Left _) = e
matchMultiple t1s e@(Right t) = fromMaybe (matchMultipleError t1s t) (lookup t t1s)
  where
    matchMultipleError t1s t2 =
      err
        $ "se esperaba "
        ++ intercalate " o " (map (render . printType . fst) t1s)
        ++ ", pero "
        ++ render (printType t2)
        ++ " fue inferido."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = maybe (notfoundError n) (ret . snd) (lookup n e)
infer' c e (t :@: u) = checkIsFun (infer' c e t) >>= \(FunT t1 t2) -> match t1 (infer' c e u) >> ret t2
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
-- Ejercicio 3
infer' c e (Let t u) = infer' c e t >>= \tt -> infer' (tt : c) e u

-- Ejercicio 4
infer' c e Zero = ret NatT
infer' c e (Suc t) = match NatT $ infer' c e t
infer' c e (Rec t1 t2 t3) = 
  infer' c e t1 >>= \tt1 ->
    matchMultiple [
      (FunT tt1 (FunT NatT tt1), match NatT (infer' c e t3)),
      (FunT NatT (FunT ListT (FunT tt1 tt1)), match ListT (infer' c e t3))
    ] (infer' c e t2) 
    >> ret tt1
-- Ejercicio 6
infer' c e Nil = ret ListT
infer' c e (Cons t1 t2) = 
  match NatT (infer' c e t1) >> match ListT (infer' c e t2) >> ret ListT

