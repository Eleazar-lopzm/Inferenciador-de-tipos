# Programa básico de Inferencia de Tipos

## Cómo se utiliza

```haskell
ghci

:l Lexer Parser TypeChecker

import Lexer
import Parser
import TypeChecker

```

## Casos de Prueba

```haskell
infer [] (parser (lexer "(+ 10 20 30)"))
infer [] (parser (lexer "(not #t)"))
infer [("x", TInt)] (parser (lexer "x"))
infer [("f", TFun [TInt] TBool)] (parser (lexer "(f 10)"))
```