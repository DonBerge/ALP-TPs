module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap, Monad (return)
                                                )
import Data.Bool (Bool(True))

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State



-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = do e <- (evalExp x)
                        update v e
                        return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then stepComm c1
                                        else stepComm c2
stepComm (Repeat b c1) = do p <- evalExp b
                            if p then stepComm (Seq c1 (Repeat b c1))
                                 else stepComm Skip
               
binOp f e1 e2 = do
                  v1 <- evalExp e1
                  v2 <- evalExp e2
                  return (f v1 v2)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var x) = lookfor x
evalExp (UMinus x) = (evalExp x) >>= (return . (negate))
evalExp (Plus x y) = binOp (+) x y
evalExp (Minus x y) = binOp (-) x y
evalExp (Times x y) = binOp (*) x y
evalExp (Div x y) = binOp (div) x y

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt x y) = binOp (<) x y
evalExp (Gt x y) = binOp (>) x y
evalExp (And x y) = binOp (&&) x y
evalExp (Or x y) = binOp (||) x y
evalExp (Not x) = (evalExp x) >>= (return . (not))
evalExp (Eq x y) = binOp (==) x y
evalExp (NEq x y) = binOp (/=) x y

