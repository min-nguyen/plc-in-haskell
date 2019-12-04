{
module ExprLex (exprLexer) where 
}

%wrapper "basic"

$alpha      = [a-zA-Z]
$alphanum   = [a-zA-z0-9]
$digit      = 0-9

tokens :-

    $white+                 ;
    $digit+                 { \s -> TokenNum (read s) }
    [\+\-\*]                { \s -> operator (head s) }
    [\=\(\)]                { \s -> delimiter (head s) }
    $alpha [$alphanum]*     { \s -> TokenVar s }
    let                     { \s -> TokenLet }
    in                      { \s -> TokenIn }
    end                     { \s -> TokenEnd }

{

operator :: Char -> Token 
operator c = case c of '+' -> TokenAdd 
                       '-' -> TokenSub 
                       '*' -> TokenMul

delimiter :: Char -> Token 
delimiter c = case c of '(' -> TokenLPar
                        ')' -> TokenRPar 
                        '=' -> TokenEq

data Token  = TokenLet 
            | TokenIn 
            | TokenEnd 
            | TokenVar String
            | TokenNum Int
            | TokenAdd
            | TokenSub
            | TokenMul
            | TokenEq 
            | TokenLPar 
            | TokenRPar
            deriving (Show, Eq)

main = do  
    s <- getContents
    print (alexScanTokens s)
}