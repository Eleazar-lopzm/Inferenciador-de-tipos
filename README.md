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
infer [] (parser (lexer "(lambda (x) x)"))
infer [] (parser (lexer "((lambda (x) (+ x 1)) #t)"))
infer [] (parser (lexer "((lambda (f) (f 10)) (lambda (x) (+ x 1)))"))
```