module Eval3
  ( eval
  , State
  )
where

import Prelude hiding (uncurry)
import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

pair:: a->b->Pair a b
pair = (:!:)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, [])

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, _) = case M.lookup v m of
                            Nothing -> Left UndefVar
                            Just a -> Right a 

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v d (m, s) = (M.insert v d m, s)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace s (m, st) = (m, st ++ s) 

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'


-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let v e) (st, s) = case evalExp e (st, s) of
                                      Left ex -> Left ex
                                      Right (e':!: s') -> Right $ pair Skip $ addTrace (unwords ["Let", v, show e', ", "]) (uncurry (update v) (e':!: s')) 

stepComm (Seq Skip c1) st = stepComm c1 st
stepComm (Seq c0 c1) st = case stepComm c0 st of
                            Left e -> Left e
                            Right (c0' :!: st') -> stepComm (Seq c0' c1) st'

stepComm Skip st = Right $ pair Skip st
stepComm (IfThenElse b c0 c1) st = case evalExp b st of
                                    Left e -> Left e
                                    Right (b' :!: st') -> if b' then Right $ pair c0 st' else Right $ pair c1 st'
stepComm (RepeatUntil c b) st = stepComm (Seq c (IfThenElse b Skip (RepeatUntil c b))) st


-----------------------------
-- Evalúa una expresión
-- Completar la definición
evalVarOp :: Variable -> (Int->Int) -> State -> Either Error (Pair Int State)
evalVarOp v op st = case lookfor v st of
                        Left e -> Left e
                        Right v' -> let nv = op v'
                                    in if v' == nv 
                                       then Right $ pair v' st
                                       else Right $ pair nv (update v nv st)

evalBinOpWithCheck :: Exp a -> Exp a -> (a->a->b) -> (a->a->Maybe Error) -> State -> Either Error (Pair b State)
evalBinOpWithCheck e0 e1 op check st = case evalExp e0 st of
                          Left e -> Left e
                          Right (n0 :!: st') -> case evalExp e1 st' of
                                                      Left e -> Left e
                                                      Right (n1 :!: st'') -> case check n0 n1 of
                                                                              Nothing -> Right $ pair (op n0 n1) st''
                                                                              Just e -> Left e
evalUnOp :: Exp a -> (a->b) -> State -> Either Error (Pair b State)
evalUnOp e0 op st = case evalExp e0 st of
                      Left e -> Left e
                      Right (n0 :!: st') -> Right $ pair (op n0) st'

evalConst :: a -> State -> Either Error (Pair a State)
evalConst a b = Right $ pair a b

checkDivByZero :: Int -> Int -> Maybe Error
checkDivByZero _ b = if b == 0 then Just DivByZero else Nothing

noCheck :: a -> a -> Maybe Error
noCheck _ _ = Nothing

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
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
evalExp (Plus e0 e1) = evalBinOpWithCheck e0 e1 (+) noCheck
evalExp (Minus e0 e1) = evalBinOpWithCheck e0 e1 (-) noCheck
evalExp (Times e0 e1) = evalBinOpWithCheck e0 e1 (*) noCheck
evalExp (Div e0 e1) = evalBinOpWithCheck e0 e1 div checkDivByZero
evalExp (Lt e0 e1) = evalBinOpWithCheck e0 e1 (<) noCheck
evalExp (Gt e0 e1) = evalBinOpWithCheck e0 e1 (>) noCheck
evalExp (And e0 e1) = evalBinOpWithCheck e0 e1 (&&) noCheck
evalExp (Or e0 e1) = evalBinOpWithCheck e0 e1 (||) noCheck
evalExp (Not e0) = evalUnOp e0 not
evalExp (Eq e0 e1) = evalBinOpWithCheck e0 e1 (==) noCheck
evalExp (NEq e0 e1) = evalBinOpWithCheck e0 e1 (/=) noCheck
