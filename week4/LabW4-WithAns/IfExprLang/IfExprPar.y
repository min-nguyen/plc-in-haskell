{
module IfExprPar where

import IfAbsyn
import IfExprLex
}

%name exprParser
%tokentype  { IfExprLex.Token }
%error      { parseError }

-- Matching terminal symbols to tokens
%token
        let     { TokenLet  }
        in      { TokenIn   }
        end     { TokenEnd  }
        num     { TokenNum $$ }
        var     { TokenVar $$ }
        '='     { TokenEq   }
        '+'     { TokenAdd  }
        '-'     { TokenSub  }
        '*'     { TokenMul  }
        '('     { TokenLPar }
        ')'     { TokenRPar }
        -- Added to support ifs:
        if      { TokenIf  }
        then    { TokenThen   }
        else    { TokenElse  }

-- Associativity of terminals
%left '-' '+'
%left '*'

%%

-- Grammar:
Expr
        -- Grammar rule:               Result of parsing:
        : var                          { Var  $1           } -- Rule 1
        | num                          { CstI $1           } -- Rule 2
        | '-' num                      { CstI (- $2)       } -- Rule 3
        | '(' Expr ')'                 { $2                } -- Rule 4
        | let var '=' Expr in Expr end { Let $2 $4 $6      } -- Rule 5
        | Expr '*' Expr                { Prim "*" $1 $3    } -- Rule 6
        | Expr '+' Expr                { Prim "+" $1 $3    } -- Rule 7
        | Expr '-' Expr                { Prim "-" $1 $3    } -- Rule 8
        -- Added to support ifs:
        | if Expr then Expr else Expr  { If $2 $4 $6       }

{

-- Parsing function:
parseError :: [Token] -> a
parseError _ = error "Parse error"

}