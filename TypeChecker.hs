module TypeChecker (Type(..), infer, TEnv) where

import Parser (ExprS(..))
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (foldM)


----------------------------------------------------
-- Tipos del lenguaje
----------------------------------------------------

data Type
  = TInt
  | TBool
  | TVar String
  | TFun [Type] Type
  deriving (Eq, Show)

type TEnv = [(String, Type)]

----------------------------------------------------
-- Sustituciones
----------------------------------------------------

type Subst = [(String, Type)]

nullSubst :: Subst
nullSubst = []

-- aplica una sustitución a un Type
class Types a where
  apply :: Subst -> a -> a
  ftv   :: a -> [String]

instance Types Type where
  apply s (TVar v) = case lookup v s of
                       Just t  -> t
                       Nothing -> TVar v
  apply s (TFun ts r) = TFun (map (apply s) ts) (apply s r)
  apply _ t = t

  ftv (TVar v)     = [v]
  ftv (TFun ts r)  = concatMap ftv ts ++ ftv r
  ftv _            = []

instance Types a => Types [a] where
  apply = map . apply
  ftv   = concatMap ftv

-- aplicar sustitución a entorno
applyEnv :: Subst -> TEnv -> TEnv
applyEnv s env = [(v, apply s t) | (v,t) <- env]

-- composición: s1 `compose` s2 significa aplicar s1 luego s2 efectivamente
compose :: Subst -> Subst -> Subst
compose s1 s2 = [(v, apply s1 t) | (v,t) <- s2] ++ s1

----------------------------------------------------
-- Monad de inferencia
----------------------------------------------------

type Infer a = ExceptT String (State Int) a

runInfer :: Infer (Subst, Type) -> Either String (Subst, Type)
runInfer m = evalState (runExceptT m) 0

fresh :: Infer Type
fresh = do
  n <- get
  modify (+1)
  return (TVar ("a" ++ show n))

----------------------------------------------------
-- Unificación
----------------------------------------------------

unify :: Type -> Type -> Infer Subst
unify (TFun as r) (TFun bs r2)
  | length as == length bs = do
      s1 <- unifyMany as bs
      s2 <- unify (apply s1 r) (apply s1 r2)
      return (s2 `compose` s1)
  | otherwise = throwError "Aridad de función incorrecta"

unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
unify TInt TInt  = return nullSubst
unify TBool TBool = return nullSubst

unify t1 t2 =
  throwError $ "No se pueden unificar: " ++ show t1 ++ " con " ++ show t2

unifyMany :: [Type] -> [Type] -> Infer Subst
unifyMany [] [] = return nullSubst
unifyMany (t:ts) (u:us) = do
  s1 <- unify t u
  s2 <- unifyMany (map (apply s1) ts) (map (apply s1) us)
  return (s2 `compose` s1)
unifyMany _ _ = throwError "Listas de distinto tamaño en unifyMany"

bindVar :: String -> Type -> Infer Subst
bindVar v t
  | t == TVar v        = return nullSubst
  | v `elem` ftv t     = throwError ("Ocurre-check falló para " ++ v)
  | otherwise          = return [(v,t)]

----------------------------------------------------
-- Inferencia: ahora devuelve (Subst, Type)
----------------------------------------------------

infer :: TEnv -> ExprS -> Either String Type
infer env expr =
  case runInfer (inferExpr env expr) of
    Left err -> Left err
    Right (s, t) -> Right (apply s t)  -- aplica la sustitución final al tipo

inferExpr :: TEnv -> ExprS -> Infer (Subst, Type)

----------------------------------------------------
-- Literales
----------------------------------------------------
inferExpr _ (NumS _)  = return (nullSubst, TInt)
inferExpr _ (BoolS _) = return (nullSubst, TBool)

----------------------------------------------------
-- Variables
----------------------------------------------------
inferExpr env (IdS x) =
  case lookup x env of
    Just t  -> return (nullSubst, t)
    Nothing -> throwError ("Variable no definida: " ++ x)

----------------------------------------------------
-- Suma: (+ e1 e2 ...)
----------------------------------------------------
inferExpr env (AddS exprs) = do
  -- inferir cada argumento acumulando sustituciones
  (sAll, ts) <- foldM go (nullSubst, []) exprs
  -- unificar todos con TInt
  sUnifs <- foldM (\sAcc t -> do
                      s' <- unify (apply sAcc t) TInt
                      return (s' `compose` sAcc)
                  ) sAll ts
  return (sUnifs, TInt)
  where
    go :: (Subst, [Type]) -> ExprS -> Infer (Subst, [Type])
    go (sAcc, tsAcc) e = do
      let env' = applyEnv sAcc env
      (sE, tE) <- inferExpr env' e
      let sNew = sE `compose` sAcc
      return (sNew, tsAcc ++ [apply sNew tE])

----------------------------------------------------
-- not
----------------------------------------------------
inferExpr env (NotS e) = do
  (s1, t1) <- inferExpr env e
  s2 <- unify (apply s1 t1) TBool
  let s = s2 `compose` s1
  return (s, TBool)

----------------------------------------------------
-- lambda: (lambda (x) body)
-- (soporta solo un parámetro como en tu AST: LambdaS [x] body)
----------------------------------------------------
inferExpr env (LambdaS [x] body) = do
  tv <- fresh
  -- inferir el cuerpo con x : tv
  let env' = (x, tv) : env
  (s1, tBody) <- inferExpr env' body
  -- el tipo del parámetro puede haber sido refinado por s1
  let paramType = apply s1 tv
  return (s1, TFun [paramType] tBody)

----------------------------------------------------
-- aplicación: (f a b ...)
----------------------------------------------------
inferExpr env (AppS f args) = do
  -- inferir f
  (sF, tF) <- inferExpr env f
  -- inferir args secuencialmente aplicando s acumulada al entorno
  (sArgs, tArgs) <- foldM go (sF, []) args
  -- fresh para resultado
  tRes <- fresh
  -- unificar (aplicar sArgs a tF)
  sUnify <- unify (apply sArgs tF) (TFun tArgs tRes)
  let sTotal = sUnify `compose` sArgs
  return (sTotal, apply sUnify tRes)
  where
    go :: (Subst, [Type]) -> ExprS -> Infer (Subst, [Type])
    go (sAcc, tsAcc) e = do
      let env' = applyEnv sAcc env
      (sE, tE) <- inferExpr env' e
      let sNew = sE `compose` sAcc
      return (sNew, tsAcc ++ [apply sNew tE])

----------------------------------------------------
-- Caso por defecto (por si hay otros constructores)
----------------------------------------------------
inferExpr _ e = throwError ("Constructo no manejado por inferencia: " ++ show e)
