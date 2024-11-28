module Eval3
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

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: Env -> (Either Error (a, Env), Trace) }

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
 pure  = return
 (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\e -> (return (x,e), ""))
  m >>= f = StateErrorTrace (\e -> let 
                                    (st,t) = runStateErrorTrace m e
                                   in case st of
                                        Left e -> (Left e, t)
                                        Right (a,e') -> let
                                                          (st', t') = runStateErrorTrace (f a) e'
                                                        in
                                                          (st', if t=="" then t' else if t'=="" then t else t++", "++t') 
                              )
                                    
-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where 
  addTrace s = StateErrorTrace (\e -> (return ((),e),s))


-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (const (Left e, "throw " ++ show e))


-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> case lookfor' v s of
                                      Nothing -> (Left UndefVar, "throw UndefVar " ++ v)
                                      Just x' -> (Right (x',s), ""))
    where lookfor' = M.lookup
  update v i = StateErrorTrace (\s -> (return ((), update' v i s), unwords [v,"<--",show i])) 
    where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo


eval :: Comm -> (Either Error Env, Trace)
eval p = let
            (st, t) = runStateErrorTrace (stepCommStar p) initEnv
         in case st of
            Left e -> (Left e, t)
            Right (_,e) -> (return e, t)


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= stepCommStar


-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 


stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then do
                                              addTrace "if-true"
                                              stepComm c1
                                        else do
                                              addTrace "if-false" 
                                              stepComm c2
stepComm c@(Repeat b c1) = do p <- evalExp b
                              if p then do
                                          addTrace "repeat"
                                          stepComm c1 >> stepComm c 
                                   else do
                                          addTrace "repeat-exit" 
                                          stepComm Skip


-- Evalua una expresion

when :: Applicative f => Bool -> f () -> f ()
when p s  = if p then s else pure ()

evalConst :: (MonadState m, MonadError m, MonadTrace m) => a -> m a
evalConst = return

evalUnOp :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->b) -> String -> Exp a -> m b
evalUnOp op showOp x = do
                        x' <- evalExp x
                        x'' <- return (op x')
                        addTrace (unwords [showOp ++ show x',"-->",show x''])
                        return x''


type Check a m = a -> a -> m ()

checkDivByZero :: (MonadError m, MonadTrace m) => Check Int m
checkDivByZero x y = when (y==0) (addTrace (show x ++ " / " ++ show y) >> throw DivByZero)

noCheck :: Applicative m => Check a m
noCheck _ _ = pure ()

evalBinOpWithCheck :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => Check a m -> (a->a->b) -> String -> Exp a -> Exp a -> m b
evalBinOpWithCheck check op showOp x y = do
                                          x' <- evalExp x
                                          y' <- evalExp y
                                          z <- return (x' `op` y')
                                          check x' y'
                                          addTrace (unwords [show x',showOp,show y',"-->",show z])
                                          return z

evalBinOp :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->a->b) -> String -> Exp a -> Exp a -> m b
evalBinOp = evalBinOpWithCheck noCheck

evalVarOp :: (MonadState m, MonadError m, MonadTrace m) => (Int->Int) -> String -> Variable -> m Int
evalVarOp op showOp v = do {
                          x <- lookfor v;
                          x' <- return (op x);
                          addTrace (unwords [v++showOp,"-->",show x']);
                          when (x/=x') (update v x');
                          return x';
                        }

evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
-- Operaciones con variables
evalExp (Var v) = evalVarOp id "" v
evalExp (VarInc v)= evalVarOp (+1) "++" v
evalExp (VarDec v) = evalVarOp (subtract 1) "--" v

-- Enteros
evalExp (Const a) = evalConst a
evalExp (UMinus x) = evalUnOp negate "-" x
evalExp (Plus x y) = evalBinOp (+) "+" x y
evalExp (Minus x y) = evalBinOp (-) "-" x y
evalExp (Times x y) = evalBinOp (*) "*" x y
-- Aqui es necesario chequear no dividir por 0
evalExp (Div x y) = evalBinOpWithCheck checkDivByZero div "/" x y

-- Booleanos
evalExp BTrue = evalConst True
evalExp BFalse = evalConst False
evalExp (Lt x y) = evalBinOp (<) "<" x y
evalExp (Gt x y) = evalBinOp (>) ">" x y
evalExp (And x y) = evalBinOp (&&) "&&" x y
evalExp (Or x y) = evalBinOp (||) "||" x y
evalExp (Not x) = evalUnOp not "!" x
evalExp (Eq x y) = evalBinOp (==) "==" x y
evalExp (NEq x y) = evalBinOp (/=) "/=" x y