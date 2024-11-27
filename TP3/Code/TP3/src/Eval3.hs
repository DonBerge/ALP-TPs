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
  StateErrorTrace { runStateErrorTrace :: Env -> Either Error (a,String,Env) }

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
 pure  = return
 (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\e -> return (x, "", e))
  m >>= f = StateErrorTrace (\e -> do (a,s,e') <- runStateErrorTrace m e
                                      (b,s',e'') <- runStateErrorTrace (f a) e'
                                      return (b, s ++ "\n" ++ s, e''))
                                      
-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where 
  addTrace s = StateErrorTrace (\e -> return ((),s,e))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (const (Left e))


-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> let p = (lookfor' v s :!: s) in
                                              case fst p of
                                                    Nothing -> Left UndefVar
                                                    Just x' -> Right (x', "", snd p)
                                                  )
    where lookfor' = M.lookup
  update v i = StateErrorTrace (\s -> return ((), "", update' v i s)) 
    where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval p = runStateErrorTrace (stepCommStar p) initEnv >>= (return . (\(_,b,c) -> (c,b)))

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm Skip = return Skip 
stepComm (Let v x) = do e <- (evalExp x)
                        update v e
                        addTrace (v ++"<--"++ (show e))
                        return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   addTrace ("if evalua a " ++ (show p))
                                   if p then stepComm c1
                                        else stepComm c2
stepComm (Repeat b c1) = do p <- evalExp b
                            if p then addTrace "Repito: " >> stepComm (Seq c1 (Repeat b c1))
                                 else stepComm Skip

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]

binOp strF f e1 e2 = do
                        v1 <- evalExp e1
                        v2 <- evalExp e2
                        addTrace (unwords [show v1, strF, show v2, "-->", show (f v1 v2)])
                        return (f v1 v2)

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var x) = do 
                    v <- lookfor x
                    addTrace (unwords ["get",x,"-->",show v])
                    return v
evalExp (UMinus x) = do 
                      v <- evalExp x
                      addTrace (unwords ["-",show v,"-->",show (-v)])
                      return (-v)
evalExp (Plus x y) = binOp "+" (+) x y
evalExp (Minus x y) = binOp "-" (-) x y
evalExp (Times x y) = binOp "*" (*) x y
evalExp (Div x y) = binOp "/" (div) x y

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt x y) = binOp "<" (<) x y
evalExp (Gt x y) = binOp ">" (>) x y
evalExp (And x y) = binOp "And" (&&) x y
evalExp (Or x y) = binOp "Or" (||) x y
evalExp (Not x) = do 
                    v <- evalExp x
                    addTrace (unwords ["Not",show v,"-->",show (not v)])
                    return (not v)
evalExp (Eq x y) = binOp "=="  (==) x y
evalExp (NEq x y) = binOp "/=" (/=) x y
