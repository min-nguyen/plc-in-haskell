{
module CPar where

import Absyn
import CLex
import Data.Maybe

compose1 f (g, s) = (\x -> g (f x), s)

nl = CstI 10
}

%name cParser
%tokentype  { CLex.Token }
%error      { parseError }


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


Main    : Topdecs eof                           { Prog $1                   }

Topdecs : {- empty -}                           { []                        }
        | Topdec Topdecs                        { $1 : $2                   }

Topdec  : Vardec semi                           { VarDec (fst $1) (snd $1)  }
        | Fundec                                { $1                        }

Vardec  : Type Vardesc                          { ((fst $2) $1, snd $2)     }

Vardesc : name                                  { (\t -> t, $1)             }
        | times Vardesc                         { compose1 TypP $2          }
        | lpar Vardesc rpar                     { $2                        }
        | Vardesc lbrack rbrack                 { compose1 (\t -> TypA t Nothing)       }
        | Vardesc lbrack cstint rbrack          { compose1 (\t -> TypA t (Just $3)) $1  }

Fundec  : void name lpar Paramdecs rpar Block   { FunDec Nothing $2 $4 $6   }
        | Type name lpar Paramdecs rpar Block   { FunDec (Just $1) $2 $4 $6 }

Paramdecs : {- empty -}                         { [] }
        | Paramdecs1                            { $1 }

Paramdecs1
        : Vardec                                { [$1]                      }
        | Vardec comma Paramdecs1               { $1 : $3                   }

Block   : lbrace StmtOrDecSeq rbrace            { Block $2                  }

StmtOrDecSeq
        : {- empty -}                           { []                            }
        | Stmt StmtOrDecSeq                     { Stmt ($1 : $2)                }
        | Vardec semi StmtOrDecSeq              { Dec ((fst $1, snd $1) : $3)   }

Stmt    : StmtM                                 { $1                    }
        | StmtU                                 { $1                    }

StmtM   : Expr semi                             { Expr $1               }
        | return semi                           { Return Nothing        }
        | return Expr semi                      { Return (Just $2)      }
        | Block                                 { $1                    }
        | if lpar Expr rpar StmtM else StmtM    { If $3 $3 $7           }
        | while lpar Expr rpar StmtM            { While $3 $5           }

StmtU   : if lpar Expr rpar StmtM else StmtU    { If $5 $5 $7           }
        | if lpar Expr rpar Stmt                { If $3 $5 (Block [])   }
        | while lpar Expr rpar StmtU            { While $3 $5           }

Expr    : Access                                { Access $1             }
        | ExprNotAccess                         { $1                    }

ExprNotAccess
        : AtExprNotAccess                       { $1                    }
        | Access assign Expr                    { Assign $1 $3          }
        | name lpar Exprs rpar                  { Call $1 $3            }
        | not Expr                              { Prim1 "!" $2          }
        | print Expr                            { Prim1 "printi" $2     }
        | println                               { Prim1 "printc" nl     }
        | Expr '+' Expr                         { Prim2 "+" $1 $3       }
        | Expr '-' Expr                         { Prim2 "-" $1 $3       }
        | Expr '*' Expr                         { Prim2 "*" $1 $3       }
        | Expr '/' Expr                         { Prim2 "/" $1 $3       }
        | Expr '%' Expr                         { Prim2 "%" $1 $3       }
        | Expr "==" Expr                        { Prim2 "==" $1 $3      }
        | Expr "!= Expr                         { Prim2 "!=" $1 $3      }
        | Expr '>' Expr                         { Prim2 ">" $1 $3       }
        | Expr '<' Expr                         { Prim2 "<" $1 $3       }
        | Expr ">=" Expr                        { Prim2 ">=" $1 $3      }
        | Expr "<=" Expr                        { Prim2 "<=" $1 $3      }
        | Expr "&&" Expr                        { AndAlso $1 $3         }
        | Expr "||" Expr                        { OrElse $1 $3          }

AtExprNotAccess
        : Const                                 { CstI $1               }
        | lpar ExprNotAccess rpar               { $2                    }
        | amp Access                            { Addr $2               }

Access  : name                                  { AccVar $1             }
        | lpar Access rpar                      { $2                    }
        | times Access                          { AccDeref (Access $2)  }
        | times AtExprNotAccess                 { AccDeref $2           }
        | Access lbrack Expr rbrack             { AccIndex $1 $3        }

Exprs   : {- empty -}                           { []                    }
        | Exprs1                                { $1                    }

Exprs1  : Expr                                  { [$1]                  }
        | Expr ',' Exprs1                       { $1 : $3               }

Const   : cstint                                { $1                    }
        | cstbool                               { $1                    }
        | '-' cstint                            { (- $2)                }
        | null                                  { (-1)                  }

Type    : int                                   { TypI                  }
        | char                                  { TypC                  }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}