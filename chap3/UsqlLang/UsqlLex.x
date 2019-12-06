{
module UsqlLex (  usqlLexer
                , Token(..)) where 
}

%wrapper "basic"

$alpha      = [a-zA-Z]
$alphanum   = [a-zA-z0-9]
$digit      = 0-9
$graphic    = $printable # $white

@string     = \' ($graphic # \')* \'

tokens :-

    $white+                                     ;
    $digit+                                     { \s -> TokenNum (read s)  }
    (\<\>|\<\=|\>\=|\=\=)                       { \s -> let (x:y:ys) = s in operator [x,y]}
    (\+|\-|\*|\/|\%|\>|\<)                      { \s -> operator ([head s])  }
    (\(|\)|\,|\.)                               { \s -> delimiter ([head s]) }
    let                                         { \s -> TokenLet }
    \-\- $graphic "\n"                          ;
    @string                                     { \s -> TokenString s}
    in                                          { \s -> TokenIn }
    and                                         { \s -> TokenAnd}
    false                                       { \s -> TokenBool False}
    true                                        { \s -> TokenBool True}
    select                                      { \s -> TokenSelect}
    or                                          { \s -> TokenOr}
    where                                       { \s -> TokenWhere}
    from                                        { \s -> TokenFrom}
    not                                         { \s -> TokenNot}
    $alpha [$alphanum]*                         { \s -> TokenName s }

{

operator :: String -> Token 
operator c = case c of "+"   -> TokenAdd 
                       "-"   -> TokenSub 
                       "*"   -> TokenMul
                       "/"   -> TokenDiv 
                       "%"   -> TokenMod
                       "=="  -> TokenEq
                       "<>"  -> TokenNE
                       ">"   -> TokenGT
                       "<"   -> TokenLT 
                       ">="  -> TokenGE 
                       "<="  -> TokenLE 


delimiter :: String -> Token 
delimiter c = case c of "("   -> TokenLPar
                        ")"   -> TokenRPar 
                        ","   -> TokenComma
                        "."   -> TokenDot 

data Token  = TokenLet 
            | TokenIn 
            | TokenEnd 
            | TokenName String
            | TokenNum Int
            | TokenAdd
            | TokenSub
            | TokenMul
            | TokenDiv
            | TokenEq 
            | TokenLPar 
            | TokenRPar
            | TokenMod 
            | TokenNE 
            | TokenGT 
            | TokenLT 
            | TokenGE 
            | TokenLE 
            | TokenDot 
            | TokenComma 
            | TokenString String
            | TokenAnd 
            | TokenBool Bool
            | TokenOr 
            | TokenFrom 
            | TokenSelect 
            | TokenWhere
            | TokenNot
            deriving (Show, Eq)

usqlLexer :: String -> [Token]
usqlLexer s = alexScanTokens s
}