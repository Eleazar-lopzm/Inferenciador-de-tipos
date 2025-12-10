module TypeChecker (Type(..), infer, TEnv) where

import Parser (ExprS(..))

----------------------------------------------------
-- Tipos del lenguaje
----------------------------------------------------

data Type
  = TInt
  | TBool
  | TFun [Type] Type    -- argumentos -> retorno
  deriving (Eq, Show)

type TEnv = [(String, Type)]

----------------------------------------------------
-- Inferencia
----------------------------------------------------

infer :: TEnv -> ExprS -> Either String Type

----------------------------------------------------
-- Literales
----------------------------------------------------
infer _ (NumS _)  = Right TInt
infer _ (BoolS _) = Right TBool

----------------------------------------------------
-- Variables
----------------------------------------------------
infer env (IdS x) =
  case lookup x env of
    Just t  -> Right t
    Nothing -> Left ("Variable no definida: " ++ x)

----------------------------------------------------
-- Suma (+ e1 e2 ...)
----------------------------------------------------
infer env (AddS exprs) = do
  ts <- mapM (infer env) exprs
  if all (== TInt) ts
    then Right TInt
    else Left "Error: (+) solo acepta enteros"

----------------------------------------------------
-- not
----------------------------------------------------
infer env (NotS e) = do
  t <- infer env e
  case t of
    TBool -> Right TBool
    _     -> Left "Error: (not e) requiere un booleano"

----------------------------------------------------
-- Lambda: (lambda (x) body)
----------------------------------------------------
infer env (LambdaS [x] body) =
  Left "Error: No se puede inferir el tipo del argumento x sin anotaciones. Agrega (x :: Tipo) en el entorno al llamar infer."

-- Si luego quieres inferencia con variables de tipo, te la hago.
-- Por ahora lo dejamos simple.

----------------------------------------------------
-- Aplicación: (f a b ...)
----------------------------------------------------
infer env (AppS fun args) = do
  tf <- infer env fun
  targs <- mapM (infer env) args
  case tf of
    TFun paramTypes retType ->
      if paramTypes == targs
        then Right retType
        else Left ("Error de tipos en aplicación. Esperado: "
                   ++ show paramTypes ++ " pero recibí " ++ show targs)
    _ ->
      Left ("Error: intento de aplicar una expresión que no es función: " ++ show tf)
