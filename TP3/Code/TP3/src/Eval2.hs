module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

import Prelude hiding (fst,snd)

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\e -> return (x :!: e))
  m >>= f = StateError (\e -> do p <- runStateError m e
                                 runStateError (f (fst p)) (snd p))

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw err = StateError (const (Left err))

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> let p = (lookfor' v s :!: s) in
                                case fst p of
                                      Nothing -> Left UndefVar
                                      Just x' -> Right (x' :!: snd p)
                                    )
    where lookfor' = M.lookup
  update v i = StateError (\s -> return (() :!: update' v i s)) 
    where update' = M.insert


-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = runStateError (stepCommStar p) initEnv >>= (return . snd)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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

when :: Applicative f => Bool -> f () -> f ()
when p s  = if p then s else pure ()

evalConst :: (MonadState m, MonadError m) => a -> m a
evalConst = return

evalUnOp :: (MonadState m, MonadError m) => (a->b) -> Exp a -> m b
evalUnOp op = (fmap op) . evalExp


type Check a m = a -> a -> m ()

checkDivByZero :: (MonadError m) => Check Int m
checkDivByZero _ y = when (y==0) (throw DivByZero)

noCheck :: Applicative m => Check a m
noCheck _ _ = pure ()

evalBinOpWithCheck :: (MonadState m, MonadError m) => Check a m -> (a->a->b) -> Exp a -> Exp a -> m b
evalBinOpWithCheck check op x y = do
                                    x' <- evalExp x
                                    y' <- evalExp y
                                    check x' y'
                                    return (x' `op` y')

evalBinOp :: (MonadState m, MonadError m) => (a->a->b) -> Exp a -> Exp a -> m b
evalBinOp = evalBinOpWithCheck noCheck

evalVarOp :: (MonadState m, MonadError m) => (Int->Int) -> Variable -> m Int
evalVarOp op v = do {
                  x <- lookfor v;
                  x' <- return (op x);
                  when (x/=x') (update v x');
                  return x';
                 }

evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
-- Aqui es necesario chequear no dividir por 0
evalExp (Div x y) = evalBinOpWithCheck checkDivByZero div x y

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


