module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

pair:: a->b->Pair a b
pair = (:!:)

-- Estado vacío
initState :: State
initState = (M.empty, [])

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, _) = case M.lookup v m of
                            Nothing -> Left UndefVar
                            Just a -> Right a 

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v d (m, s) = (M.insert v d m, s)

-- Agrega una traza dada al estado
-- La instrucción s se agrega al reves a la traza para utilizar eficientemente
-- el operador (++), ya que el costo de la operacion (s ++ t) depende del largo de s 
addTrace :: String -> State -> State
addTrace s (m, st) = (m, " ," ++ (reverse s ++ st)) 

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- Como las trazas se agregan al reves, hay que hacer un reverse al final
-- Descartamos los ultimos 2 caracteres ya que son el separador ", " y no son necesarios
-- al final de la traza
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip (m,s) = return (m, (reverse . drop 2) s)
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'


-- Evalúa un paso de un comando en un estado dado
-- unwords: crea un string a partir de una lista de strings, poniendo espacios en medio
-- unwords ["aa","bb","cc"] = "aa bb cc"
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let v e) st = case evalExp e st of
                              Left ex -> Left ex
                              Right (e' :!: s') -> Right $ pair Skip $ addTrace (unwords ["Let", v, show e']) $ update v e' s' 

stepComm (Seq Skip c1) st = stepComm c1 st
stepComm (Seq c0 c1) st = case stepComm c0 st of
                            Left e -> Left e
                            Right (c0' :!: st') -> stepComm (Seq c0' c1) st'

stepComm Skip st = Right $ pair Skip st
stepComm (IfThenElse b c0 c1) st = case evalExp b st of
                                    Left e -> Left e
                                    Right (b' :!: st') -> Right $ pair (if b' then c0 else c1) st'
stepComm (RepeatUntil c b) st = stepComm (Seq c (IfThenElse b Skip (RepeatUntil c b))) st


-----------------------------

-- Evalua a una expresion constante
evalConst :: a -> State -> Either Error (Pair a State)
evalConst a b = Right $ pair a b

{-
  e0: Expresion a evaluar
  op: Operacion a realizar
  st: Estado actual

  Evalua una operacion unaria sobre una expresion dada, actualizando el estado luego de realizar esa operacion.
  Si la evaluacion devuelve un error, se devuelve el error correspondiente.
-}
evalUnOp :: Exp a -> (a->b) -> State -> Either Error (Pair b State)
evalUnOp e0 op st = case evalExp e0 st of -- evaluo e0, si devuelve error, devuelvo el error
                      Left e -> Left e
                      Right (n0 :!: st') -> Right $ pair (op n0) st'

{- 
  Evalua una operacion unaria sobre una variable entera, actualizando el estado de esa variable
  luego de realizar esa operacion
-}
evalVarOp :: Variable -> (Int->Int) -> State -> Either Error (Pair Int State)
evalVarOp v op st = case lookfor v st of
                        Left e -> Left e -- si la variable no esta definida, devuelvo el error
                        Right v' -> let nv = op v' -- realizo la operacion unaria sobre la variable
                                    in if v' == nv 
                                       then Right $ pair v' st -- no realizo un update si el valor de la variable no cambia
                                       else Right $ pair nv (update v nv st) -- caso contrario, actualizo el estado de la variable
{-
  e0, e1: Dos expresiones a evaluar
  op: La operación a realizar
  check: Esta funcion chequea una condicion sobre la evaluacion de e0 y e1. 
         Devuelve Nothing si la condicion se cumple, y Just Error si no se cumple.
  st: Estado actual

  la funcion evalBinOpWithCheck devuelve Left Error si la evaluacion de e0 o e1 devuelve un error
  o si la funcion check devuelve Just Error, en dicho caso devuelve el error correspondiente.
  Caso contrario devuelve Right st, donde st es el nuevo estado de la evaluacion.
-}
evalBinOpWithCheck :: Exp a -> Exp a -> (a->a->b) -> (a->a->Maybe Error) -> State -> Either Error (Pair b State)
evalBinOpWithCheck e0 e1 op check st = case evalExp e0 st of -- evaluo e0, si devuelve error, devuelvo el error
                                        Left e -> Left e
                                        Right (n0 :!: st') -> case evalExp e1 st' of -- evaluo e1, si devuelve error, devuelvo el error
                                                                    Left e -> Left e
                                                                    Right (n1 :!: st'') -> case check n0 n1 of -- checkeo la condicion, si devuelve error, devuelvo el error
                                                                                            Nothing -> Right $ pair (op n0 n1) st''
                                                                                            Just e -> Left e

-- Evalua la division de dos expresiones enteras, chequeando que no ocurra una división por cero
evalDiv :: Exp Int -> Exp Int -> State -> Either Error (Pair Int State)
evalDiv e0 e1 = evalBinOpWithCheck e0 e1 div checkDivByZero
  where
    checkDivByZero _ b = if b == 0 then Just DivByZero else Nothing

-- Evalua una operacion binaria sobre dos expresiones
evalBinOp :: Exp a -> Exp a -> (a -> a -> b) -> State -> Either Error (Pair b State)
evalBinOp e0 e1 op = evalBinOpWithCheck e0 e1 op noCheck
  where
    noCheck _ _ = Nothing


-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const a) = evalConst a
evalExp (UMinus e) = evalUnOp e negate

-- Var
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
evalExp (Div e0 e1) = evalDiv e0 e1
evalExp (Lt e0 e1) = evalBinOp e0 e1 (<)
evalExp (Gt e0 e1) = evalBinOp e0 e1 (>)
evalExp (And e0 e1) = evalBinOp e0 e1 (&&)
evalExp (Or e0 e1) = evalBinOp e0 e1 (||)
evalExp (Not e0) = evalUnOp e0 not
evalExp (Eq e0 e1) = evalBinOp e0 e1 (==)
evalExp (NEq e0 e1) = evalBinOp e0 e1 (/=)
