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
stepCommStar c    = stepComm c >>= stepCommStar

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then stepComm c1
                                        else stepComm c2
stepComm c@(Repeat b c1) = do p <- evalExp b
                              if p then stepComm c1 >> stepComm c 
                                   else stepComm Skip

-- Evalua una expresion
-- Adaptamos las funciones del tp1 para el evaluador monadico

when :: Applicative f => Bool -> f () -> f ()
when p s  = if p then s else pure ()

liftA2 :: Applicative f => (a->b->c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)

evalConst :: MonadState m => a -> m a
evalConst = return

evalUnOp :: MonadState m => (a->b) -> Exp a -> m b
evalUnOp op = (fmap op) . evalExp

evalBinOp :: MonadState m => (a->a->b) -> Exp a -> Exp a -> m b
evalBinOp op x y = liftA2 op (evalExp x) (evalExp y)

evalVarOp :: MonadState m => (Int->Int) -> Variable -> m Int
evalVarOp op v = do {
                  x <- lookfor v;
                  x' <- return (op x);
                  when (x/=x') (update v x');
                  return x';
                 }

evalExp :: MonadState m => Exp a -> m a
-- Operaciones con variables
evalExp (Var v) = evalVarOp id v
evalExp (VarInc v)= evalVarOp (+1) v
evalExp (VarDec v) = evalVarOp (subtract 1) v

-- Enteros
evalExp (Const a) = evalConst a
evalExp (UMinus x) = evalUnOp negate x
evalExp (Plus x y) = evalBinOp (+) x y
evalExp (Minus x y) = evalBinOp (-) x y
evalExp (Times x y) = evalBinOp (*) x y
evalExp (Div x y) = evalBinOp (div) x y

-- Booleanos
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Lt x y) = evalBinOp (<) x y
evalExp (Gt x y) = evalBinOp (>) x y
evalExp (And x y) = evalBinOp (&&) x y
evalExp (Or x y) = evalBinOp (||) x y
evalExp (Not x) = evalUnOp not x
evalExp (Eq x y) = evalBinOp (==) x y
evalExp (NEq x y) = evalBinOp (/=) x y

