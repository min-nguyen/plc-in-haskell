{
module CPar where

import Absyn
import CLex
import Data.Maybe
}

%name cParser
%tokentype  { CLex.Token }
%error      { parseError }

%right '='
%nonassoc print
%left "||"
%left "&&"
%left "==" "!="
%nonassoc '>' '<' ">=" "<="
%left '+' '-'
%left '*' '/' '%'
%nonassoc '!' '&'
%nonassoc '['


%token
        cstint      { TokenCstInt $$        }
        cstbool     { TokenCstBool $$       }
        cststring   { TokenCstString $$     }
        name        { TokenName $$          }
        char        { TokenChar             }
        else        { TokenElse             }
        if          { TokenIf               }
        int         { TokenInt              }
        null        { TokenNull             }
        print       { TokenPrint            }
        println     { TokenPrintln          }
        return      { TokenReturn           }
        void        { TokenVoid             }
        while       { TokenWhile            }

        '+'         { TokenPlus             }
        '-'         { TokenMinus            }
        '*'         { TokenTimes            }
        '/'         { TokenDiv              }
        '%'         { TokenMod              }

        "=="        { TokenEQ               }
        "!="        { TokenNE               }
        '>'         { TokenGT               }
        '<'         { TokenLT               }
        ">="        { TokenGE               }
        "<="        { TokenLE               }

        '!'         { TokenNot              }
        "||"        { TokenSeqOr            }
        "&&"        { TokenSeqAnd           }

        '('         { TokenLPar             }
        ')'         { TokenRPar             }
        '{'         { TokenLBrace           }
        '}'         { TokenRBrace           }
        '['         { TokenLBrack           }
        ']'         { TokenRBrack           }
        ';'         { TokenSemi             }
        ','         { TokenComma            }
        '='         { TokenAssign           }
        '&'         { TokenAmp              }

        eof         { TokenEOF              }

%%

Main    : Topdecs eof                           { Prog $1                   }

Topdecs : {- empty -}                           { []                        }
        | Topdec Topdecs                        { ($1 : $2)                   }

Topdec  : Vardec ';'                            { VarDec (fst $1) (snd $1)  }
        | Fundec                                { $1                        }

Vardec  : Type Vardesc                          { ((fst $2) $1, snd $2)     }

Vardesc : name                                  { (\t -> t, $1)             }
        | '*' Vardesc                           { compose1 TypP $2          }
        | '(' Vardesc ')'                       { $2                        }
        | Vardesc '[' ']'                       { compose1 (\t -> TypA t Nothing) $1    }
        | Vardesc '[' cstint ']'                { compose1 (\t -> TypA t (Just $3)) $1  }

Fundec  : void name '(' Paramdecs ')' Block     { FunDec Nothing $2 $4 $6   }
        | Type name '(' Paramdecs ')' Block     { FunDec (Just $1) $2 $4 $6 }

Paramdecs : {- empty -}                         { [] }
        | Paramdecs1                            { $1 }

Paramdecs1
        : Vardec                                { [$1]                      }
        | Vardec ',' Paramdecs1                 { ($1 : $3)                   }

Block   : '{' StmtOrDecSeq '}'                  { Block $2                  }

StmtOrDecSeq
        : {- empty -}                           { []                            }
        | Stmt StmtOrDecSeq                     { ((Stmt $1) : $2)                }
        | Vardec ';' StmtOrDecSeq               { ((Dec (fst $1) (snd $1)) : $3)   }

Stmt    : StmtM                                 { $1                    }
        | StmtU                                 { $1                    }

StmtM   : Expr ';'                              { Expr $1               }
        | return ';'                            { Return Nothing        }
        | return Expr ';'                       { Return (Just $2)      }
        | Block                                 { $1                    }
        | if '(' Expr ')' StmtM else StmtM      { If $3 $5 $7           }
        | while '(' Expr ')' StmtM              { While $3 $5           }

StmtU   : if '(' Expr ')' StmtM else StmtU      { If $3 $5 $7           }
        | if '(' Expr ')' Stmt                  { If $3 $5 (Block [])   }
        | while '(' Expr ')' StmtU              { While $3 $5           }

Expr    : Access                                { Access $1             }
        | ExprNotAccess                         { $1                    }

ExprNotAccess
        : AtExprNotAccess                       { $1                    }
        | Access '=' Expr                       { Assign $1 $3          }
        | name '(' Exprs ')'                    { Call $1 $3            }
        | '!' Expr                              { Prim1 "!" $2          }
        | print Expr                            { Prim1 "printi" $2     }
        | println                               { Prim1 "printc" nl     }
        | Expr '+' Expr                         { Prim2 "+" $1 $3       }
        | Expr '-' Expr                         { Prim2 "-" $1 $3       }
        | Expr '*' Expr                         { Prim2 "*" $1 $3       }
        | Expr '/' Expr                         { Prim2 "/" $1 $3       }
        | Expr '%' Expr                         { Prim2 "%" $1 $3       }
        | Expr "==" Expr                        { Prim2 "==" $1 $3      }
        | Expr "!=" Expr                        { Prim2 "!=" $1 $3      }
        | Expr '>' Expr                         { Prim2 ">" $1 $3       }
        | Expr '<' Expr                         { Prim2 "<" $1 $3       }
        | Expr ">=" Expr                        { Prim2 ">=" $1 $3      }
        | Expr "<=" Expr                        { Prim2 "<=" $1 $3      }
        | Expr "&&" Expr                        { AndAlso $1 $3         }
        | Expr "||" Expr                        { OrElse $1 $3          }

AtExprNotAccess
        : Const                                 { CstI $1               }
        | '(' ExprNotAccess ')'                 { $2                    }
        | '&' Access                            { Addr $2               }

Access  : name                                  { AccVar $1             }
        | '(' Access ')'                        { $2                    }
        | '*' Access                            { AccDeref (Access $2)  }
        | '*' AtExprNotAccess                   { AccDeref $2           }
        | Access '[' Expr ']'                   { AccIndex $1 $3        }

Exprs   : {- empty -}                           { []                    }
        | Exprs1                                { $1                    }

Exprs1  : Expr                                  { [$1]                  }
        | Expr ',' Exprs1                       { ($1 : $3)             }

Const   : cstint                                { $1                    }
        | cstbool                               { $1                    }
        | '-' cstint                            { (- ($2))              }
        | null                                  { (-1)                  }

Type    : int                                   { TypI                  }
        | char                                  { TypC                  }

{

compose1 f (g, s) = (\x -> g (f x), s)

nl = CstI 10

parseError :: [Token] -> a
parseError _ = error "Parse error"

}