{
module CLex (   cLexer
            ,   Token(..)) where
}

%wrapper "basic"

$alpha      = [a-zA-Z]
$alphanum   = [a-zA-z0-9]
$digit      = 0-9
$graphic    = $printable # $white

@string     = \" ($graphic # \')* \"

tokens :-
    $white+                                ;
    $digit+                                { \s -> TokenCstInt (read s)  }
    $alpha [$alphanum]*                    { \s -> keyword s }
    (\=\=|\!\=|\>\=|\<\=|\|\||\&\&)        { \s -> let (x:y:ys) = s in operator [x,y]}
    (\+|\-|\*|\/|\%|\>|\<|\=|\&|\!)        { \s -> operator ([head s])  }
    (\(|\)|\{|\}|\[|\]|\;|\,)              { \s -> delimiter ([head s]) }
    "/*" [. \n]* "*/"                         ;
    "//".*                                 ;
    @string                                { \s -> TokenCstString s }
    eof                                    { \s -> TokenEOF }
{

operator :: String -> Token
operator c = case c of "+"   -> TokenPlus
                       "-"   -> TokenMinus
                       "*"   -> TokenTimes
                       "/"   -> TokenDiv
                       "%"   -> TokenMod
                       "="   -> TokenAssign
                       "=="  -> TokenEQ
                       "!="  -> TokenNE
                       ">"   -> TokenGT
                       "<"   -> TokenLT
                       ">="  -> TokenGE
                       "<="  -> TokenLE
                       "||"  -> TokenSeqOr
                       "&&"  -> TokenSeqAnd
                       "&"   -> TokenAmp
                       "!"   -> TokenNot

delimiter :: String -> Token
delimiter s =
    case s of
        "("     -> TokenLPar
        ")"     -> TokenRPar
        "{"     -> TokenLBrace
        "}"     -> TokenRBrace
        "["     -> TokenLBrack
        "]"     -> TokenRBrack
        ";"     -> TokenSemi
        ","     -> TokenComma

keyword :: String -> Token
keyword s =
    case s of
        "char"    -> TokenChar
        "else"    -> TokenElse
        "false"   -> TokenCstBool 0
        "if"      -> TokenIf
        "int"     -> TokenInt
        "null"    -> TokenNull
        "print"   -> TokenPrint
        "println" -> TokenPrintln
        "return"  -> TokenReturn
        "true"    -> TokenCstBool 1
        "void"    -> TokenVoid
        "while"   -> TokenWhile
        _         -> TokenName s

data Token =
          TokenCstInt Int
        | TokenCstString String
        | TokenCstBool Int
        | TokenPlus
        | TokenMinus
        | TokenTimes
        | TokenDiv
        | TokenMod
        | TokenAssign
        | TokenEQ
        | TokenNE
        | TokenGT
        | TokenLT
        | TokenGE
        | TokenLE
        | TokenSeqOr
        | TokenSeqAnd
        | TokenAmp
        | TokenNot
        | TokenLPar
        | TokenRPar
        | TokenLBrace
        | TokenRBrace
        | TokenLBrack
        | TokenRBrack
        | TokenSemi
        | TokenComma
        | TokenEOF
        | TokenChar
        | TokenElse
        | TokenIf
        | TokenInt
        | TokenNull
        | TokenPrint
        | TokenPrintln
        | TokenReturn
        | TokenVoid
        | TokenWhile
        | TokenName String
        deriving (Show, Eq)

funLexer :: String -> [Token]
funLexer s = alexScanTokens s


cEscape :: String -> Char
cEscape s =
    case s of
        "\\\\" -> '\\'
        "\\\"" -> '\"'
        "\\a"  -> '\007'
        "\\b"  -> '\008'
        "\\t"  -> '\t'
        "\\n"  -> '\n'
        "\\v"  -> '\011'
        "\\f"  -> '\012'
        "\\r"  -> '\r'
        _      -> error "Lexer error: impossible C escape"

cLexer :: String -> [Token]
cLexer s = alexScanTokens s
}