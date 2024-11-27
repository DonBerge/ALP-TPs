module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let t u) = sep [text "let", pp ii vs t , text "in", pp (ii+1) vs u]
pp ii vs t
  | isNat t = printNat ii vs t
  | otherwise = printList ii vs t

applyParen :: Int -> [String] -> Term -> Doc
applyParen ii vs t = parensIf (not $ isAtom t) (pp ii vs t)

printNat :: Int -> [String] -> Term -> Doc
printNat ii vs Zero = text "0"
printNat ii vs (Suc t@(Suc _)) = sep[text "suc", pp ii vs t] -- No poner parentesis en cadenas de suc
printNat ii vs (Suc t) = sep [text "suc", applyParen ii vs t]
printNat ii vs (Rec t u v) = sep [text "R", applyParen ii vs t, applyParen ii vs u, applyParen ii vs v]
printNat ii vs t = pp ii vs t

printList :: Int -> [String] -> Term -> Doc
printList ii vs Nil = text "nil"
printList ii vs (Cons t u) = sep [text "cons", applyParen ii vs t, applyParen ii vs u]
printList ii vs (Rec t u v) = sep [text "RL", applyParen ii vs t, applyParen ii vs u, applyParen ii vs v]
printList ii vs t = pp ii vs t

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isNat :: Term -> Bool
isNat Zero = True
isNat (Suc _) = True
isNat (Rec _ _ v) = isNat v
isNat _ = False

isAtom :: Term -> Bool
isAtom Zero = True
isAtom Nil = True
isAtom _ = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
-- Ejercicio 4
printType NatT = text "Nat"
-- Ejercicio 6
printType ListT = text "List Nat"

isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
-- Ejercicio 3
fv (Let t u) = fv t ++ fv u
-- Ejercicio 4
fv Zero = []
fv (Suc t) = fv t
fv (Rec t u v) = fv t ++ fv u ++ fv v
-- Ejercicio 6
fv Nil = []
fv (Cons t u) = fv t ++ fv u

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> v `notElem` fv t) vars) t

