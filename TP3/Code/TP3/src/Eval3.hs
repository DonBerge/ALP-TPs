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
                                                          -- Por eficiencia meto las trazas al reves y las doy vuelta al terminar la ejecucion
                                                          (
                                                            st', if t=="" 
                                                                  then t' 
                                                                  else if t'=="" 
                                                                    then t 
                                                                    else t' ++" ,"++ t
                                                          ) 
                              )
                                    
-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.

-- Añade una traza a la ejecucion
-- Las trazas se agregan al reves por eficiencia, al final de la evaluacion se invierten
instance MonadTrace StateErrorTrace where 
  addTrace s = StateErrorTrace (\e -> (return ((),e),reverse s))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.

-- Devuelve el error sin agregar la traza
throw':: Error -> StateErrorTrace a
throw' e = StateErrorTrace (const (Left e, ""))

instance MonadError StateErrorTrace where
  throw e = addTrace ("throw" ++ show e) >> throw' e

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> runStateErrorTrace (lookfor' v s) s)
    where lookfor' v s = case M.lookup v s of
                          Nothing -> addTrace (v ++ " --> ???") >> addTrace ("throw UndefVar " ++ v) >> throw' UndefVar
                          Just x' -> return x'

  -- unwords: toma una lista de strings y devuelve un nuevo string con los elementos de la lista separados por espacios
  -- Agrega una traza de actualizacion de una variable y actualiza el estado
  update v i = addTrace (unwords [v,"<--",show i]) >> update' v i
    where 
      -- Actualiza el estado con el nuevo valor de la variable
      update' v i = StateErrorTrace (\s -> (Right ((), M.insert v i s), "")) 

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.

-- Evalua un programa en el estado nulo
-- Si no hay errores se devuelve el estado al final del programa
-- Tambien se devuelve la traza de ejecución, como la misma se guardo al reves,
-- hay que darla vuelta
eval :: Comm -> (Either Error Env, Trace)
eval p = let
            (st, t) = runStateErrorTrace (stepCommStar p) initEnv
            -- Doy vuelta la traza
            t' = reverse t
         in case st of
            -- Hubo un error, lo devuelvo junto con la traza
            Left e -> (Left e, t')
            -- Obtengo el estado final y lo devuelvo junto a la traza
            Right (_,e) -> (return e, t')


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= stepCommStar


-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let v x) = evalExp x >>= update v >> return Skip
stepComm (Seq c1 c2) = stepComm c1 >> stepComm c2 
stepComm (IfThenElse b c1 c2) = do p <- evalExp b
                                   if p then addTrace "if-true"  >> stepComm c1
                                        else addTrace "if-false" >> stepComm c2
stepComm r@(Repeat b c) = do p <- evalExp b
                             if p then addTrace "repeat-loop" >> stepComm c >> stepComm r 
                                  else addTrace "repeat-exit" >> stepComm Skip


-- Evalua una expresion

-- Si el predicado es verdadero ejecuta la expresion s, sino no ejecuta nada
-- Inspirada en la funcion homonima de Control.Monad
when :: Applicative f => Bool -> f () -> f ()
when p s  = if p then s else pure ()

-- Evalua a una expresion constante
evalConst :: (MonadState m, MonadError m, MonadTrace m) => a -> m a
evalConst = return

{-
  op: Operacion a realizar
  showOp: Represntacion de la operacion como string
  x: Expresion sobre la que se realiza la operacion

  Evalua una operacion unaria sobre una expresion dada, actualizando la traza de ejecucion y capturando errores si los hay.
-}
evalUnOp :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->b) -> String -> Exp a -> m b
evalUnOp op showOp x = do
                        x' <- evalExp x
                        let x'' = op x' in do
                          addTrace (unwords [showOp ++ "(" ++ show x' ++ ")","-->",show x''])
                          return x''


-- Funcion que chequea una condicion sobre 2 valores
type Check a m = a -> a -> m ()

-- Chequea que no se realize una division por 0, devolviendo un error si ocurre
checkDivByZero :: (MonadError m, MonadTrace m) => Check Int m
checkDivByZero x y = when (y==0) (addTrace (show x ++ " / " ++ show y) >> throw DivByZero)

-- No realiza ningun chequeo
noCheck :: Applicative m => Check a m
noCheck _ _ = pure ()

{-
  check: Esta funcion chequea una condicion sobre la evaluacion de x e y, realizando efectos si se cumple o no.
  Por ejemplo, se puede chequear que y no sea 0 al hacer una division.

  op: Operacion binaria a realizar
  showOp: Representacion de op como string

  x,y: Dos expresiones sobre las que se realiza una expresion luego de evaluarlas

  Evalua dos expresiones, realiza un chequeo sobre ellas, actualiza la traza de ejecucion y devuelve el resultado de realizar op
  sobre los valores obtenidos de las expresiones.
-}
evalBinOpWithCheck :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => Check a m -> (a->a->b) -> String -> Exp a -> Exp a -> m b
evalBinOpWithCheck check op showOp x y = do
                                          x' <- evalExp x
                                          y' <- evalExp y
                                          check x' y'
                                          let z = x' `op` y' in do
                                            addTrace (unwords [show x',showOp,show y',"-->",show z])
                                            return z

{-
  Igual que evalBinOpWithCheck, pero no realiza un chequeo
-}
evalBinOp :: (MonadState m, MonadError m, MonadTrace m, Show a, Show b) => (a->a->b) -> String -> Exp a -> Exp a -> m b
evalBinOp = evalBinOpWithCheck noCheck

{- 
  Evalua una operacion unaria sobre una variable entera, actualizando el estado de esa variable y la traza de ejecución
-}
evalVarOp :: (MonadState m, MonadError m, MonadTrace m) => (Int->Int) -> String -> Variable -> m Int
evalVarOp op showOp v = do {
                          x <- lookfor v;
                          let x' = op x in do
                            addTrace (unwords [v++showOp,"-->",show x']);
                            when (x/=x') (update v x');
                            return x';
                        }

-- Evalua una expresion
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