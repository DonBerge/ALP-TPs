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



-- Evalua una expresion

binOp f e1 e2 = do
                  v1 <- evalExp e1
                  v2 <- evalExp e2
                  return (f v1 v2)

evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var x) = lookfor x
evalExp (UMinus x) = (evalExp x) >>= (return . (negate))
evalExp (Plus x y) = binOp (+) x y
evalExp (Minus x y) = binOp (-) x y
evalExp (Times x y) = binOp (*) x y
evalExp (Div x y) = do v1 <- evalExp x
                       v2 <- evalExp y
                       if v2 == 0 then throw DivByZero
                                 else return (div v1 v2)

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt x y) = binOp (<) x y
evalExp (Gt x y) = binOp (>) x y
evalExp (And x y) = binOp (&&) x y
evalExp (Or x y) = binOp (||) x y
evalExp (Not x) = (evalExp x) >>= (return . (not))
evalExp (Eq x y) = binOp (==) x y
evalExp (NEq x y) = binOp (/=) x y 
-- evalExp (EAssgn s x)
-- evalExp (Eseq x y)


