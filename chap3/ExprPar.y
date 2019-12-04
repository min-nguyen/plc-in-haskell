{
module ExprPar where

import Absyn

}

%name happyParser
%tokentype  { Token }
%error      { parseError }


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

%%

Expr    : let var '=' Expr in Expr  { Let $2 $4 $6      }
        | Expr '+' Expr             { Prim "+" $1 $3    }
        | Expr '-' Expr             { Prim "-" $1 $3    }
        | Expr '*' Expr             { Prim "*" $1 $3    }

        
{

parseError :: [Token] -> a 
parseError _ = error "Parse error"

main = getContents >>= print . happyParser . exprLexer
}