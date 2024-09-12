module Eval1
  ( eval
  , State
  )
where

import Prelude hiding (uncurry)
import           AST
import qualified Data.Map.Strict               as M
import Data.Strict.Tuple
-- Estados
type State = M.Map Variable Int

pair:: a->b->Pair a b
pair = (:!:)

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor = flip (M.!)

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Let v e) st = pair Skip $ uncurry (update v) $ evalExp e st
stepComm (Seq Skip c1) st = stepComm c1 st
stepComm (Seq c0 c1) st = let
                            (c0' :!: st') = stepComm c0 st
                          in
                            stepComm (Seq c0' c1) st'
stepComm Skip st = pair Skip st
stepComm (IfThenElse b c0 c1) st = let
                                    (b' :!: st') = evalExp b st
                                  in
                                    if b' then pair c0 st' else pair c1 st'
stepComm (RepeatUntil c b) st = stepComm (Seq c (IfThenElse b Skip (RepeatUntil c b))) st

-- Evalúa una expresión
-- Completar la definición

evalUnOp :: Exp a -> (a->b) -> State -> Pair b State
evalUnOp e0 op st = let
                      (n0 :!: st') = evalExp e0 st
                    in
                      pair (op n0) st'

evalBinOp :: Exp a -> Exp a -> (a->a->b) -> State -> Pair b State
evalBinOp e0 e1 op st = let
                          (n0 :!: st') = evalExp e0 st
                          (n1 :!: st'') = evalExp e1 st'
                        in
                          pair (op n0 n1) st''

evalVarOp :: Variable -> (Int->Int) -> State -> Pair Int State
evalVarOp v op st = let
                      v' = lookfor v st
                      nv = op v'
                    in
                      if v' == nv 
                        then pair v' st
                        else pair nv (update v nv st)

evalConst :: a -> State -> Pair a State
evalConst = pair

evalExp :: Exp a -> State -> Pair a State
evalExp (Const a) = evalConst a
evalExp (UMinus e) = evalUnOp e negate

evalExp (Var v) = evalVarOp v id
-- VarInc
evalExp (VarInc v)= evalVarOp v (+1)

-- VarDec
evalExp (VarDec v) = evalVarOp v (subtract 1)
-- BVal
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Plus e0 e1) = evalBinOp e0 e1 (+)
evalExp (Minus e0 e1) = evalBinOp e0 e1 (-)
evalExp (Times e0 e1) = evalBinOp e0 e1 (*)
evalExp (Div e0 e1) = evalBinOp e0 e1 div
evalExp (Lt e0 e1) = evalBinOp e0 e1 (<)
evalExp (Gt e0 e1) = evalBinOp e0 e1 (>)
evalExp (And e0 e1) = evalBinOp e0 e1 (&&)
evalExp (Or e0 e1) = evalBinOp e0 e1 (||)
evalExp (Not e0) = evalUnOp e0 not
evalExp (Eq e0 e1) = evalBinOp e0 e1 (==)
evalExp (NEq e0 e1) = evalBinOp e0 e1 (/=)
