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
pp ii vs Zero = text "0"
pp ii vs (Suc t) = text "suc " <> pp ii vs t
pp ii vs (Rec t u v) = (if isNat v then text "R " else text "RL ") <> parens (pp ii vs t) <> parens (pp ii vs u) <> parens (pp ii vs v)
pp ii vs Nil = text "nil"
pp ii vs (Cons t u) = text "cons " <> parens (pp ii vs t) <> text " " <> parens (pp ii vs u)
pp ii vs (Let t u) = text "let " <> pp ii vs t <> text " in " <> pp (ii+1) vs u

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

