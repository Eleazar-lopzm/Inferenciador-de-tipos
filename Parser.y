{
module Parser (parser, ExprS(..)) where

import Lexer (Token(..))
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    int        { TokenInt $$ }
    bool       { TokenBool $$ }
    var        { TokenVar $$ }
    '('        { TokenLPar }
    ')'        { TokenRPar }
    '+'        { TokenOp "+" }
    'not'      { TokenNot }
    'lambda'   { TokenLambda }

%%

ExprS
    : int                      { NumS $1 }
    | bool                     { BoolS $1 }
    | var                      { IdS $1 }

    -- ( + e1 e2 ... )
    | '(' '+' ExprList ')'     { AddS $3 }

    -- (not e)
    | '(' 'not' ExprS ')'      { NotS $3 }

    -- (lambda (x) body)
    | '(' 'lambda' '(' var ')' ExprS ')'  { LambdaS [$4] $6 }

    -- aplicación: (f a b)
    | '(' ExprS ExprList ')'   { AppS $2 $3 }

ExprList
    : ExprS                    { [$1] }
    | ExprS ExprList           { $1 : $2 }



{
parseError :: [Token] -> a
parseError toks = error ("Error de sintaxis cerca de: " ++ show toks)

-- AST mínimo
data ExprS
  = NumS Int
  | BoolS Bool
  | IdS String
  | AddS [ExprS]
  | NotS ExprS
  | LambdaS [String] ExprS
  | AppS ExprS [ExprS]
  deriving (Show, Eq)
}