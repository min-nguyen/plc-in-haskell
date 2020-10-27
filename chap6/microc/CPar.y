{
module CPar where

import Absyn
import CLex
}

%name cParser
%tokentype  { CLex.Token }
%error      { parseError }


%token
        cstint      { TokenCstInt $$    }
        cstbool     { TokenCstBool $$   }
        cststring   { TokenCstString $$ }
        char        { TokenChar         }
        else        { TokenElse         }
        if          { TokenIf           }
        int         { TokenInt          }
        null        { TokenNull         }
        print       { TokenPrint        }
        println     { TokenPrintln      }
        return      { TokenReturn       }
        void        { TokenVoid         }
        while       { TokenWhile        }

        '+'     { TokenPlus             }
        '-'     { TokenMinus            }
        '*'     { TokenTimes            }
        '/'     { TokenDiv              }
        '%'     { TokenMod              }

        "=="    { TokenEQ               }
        "!="    { TokenNE               }
        '>'     { TokenGT               }
        '<'     { TokenLT               }
        ">="    { TokenGE               }
        "<="    { TokenLE               }

        '!'     { TokenNot              }
        "||"    { TokenSeqOr            }
        "&&"    { TokenSeqAnd           }

        '('     { TokenLPar             }
        ')'     { TokenRPar             }
        '{'     { TokenLBrace           }
        '}'     { TokenRBrace           }
        '['     { TokenLBrack           }
        ']'     { TokenRBrack           }
        ';'     { TokenSemi             }
        ','     { TokenComma            }
        '='     { TokenAssign           }
        '&'     { TokenAmp              }

        eof     { TokenEOF              }
