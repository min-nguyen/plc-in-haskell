{
module ExprPar where

import Absyn
import ExprLex
}

%name exprParser
%tokentype  { ExprLex.Token }
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

-- Associativity of terminals
%left '-' '+'
%left '*'

%%

-- Grammar:
Expr
        -- Grammar rule:               Result of parsing:
        : var                          { Var  $1           }
        | num                          { CstI $1           }
        | '-' num                      { CstI (- $2)       }
        | '(' Expr ')'                 { $2                }
        | let var '=' Expr in Expr end { Let $2 $4 $6      }
        | Expr '*' Expr                { Prim "*" $1 $3    }
        | Expr '+' Expr                { Prim "+" $1 $3    }
        | Expr '-' Expr                { Prim "-" $1 $3    }

{

-- Parsing function:
parseError :: [Token] -> a
parseError _ = error "Parse error"

}