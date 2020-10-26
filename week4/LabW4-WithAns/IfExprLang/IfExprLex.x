{
module IfExprLex (
      exprLexer
    , Token(..)
    ) where
}

%wrapper "basic"

-- Types of atoms:
$alpha      = [a-zA-Z]
$alphanum   = [a-zA-z0-9]
$digit      = 0-9

-- Mapping to tokens
tokens :-
    -- Reg ex:               -- Function mapping matched sequence to token:
    $white+                 ;
    $digit+                 { \s -> TokenNum (read s) }
    [\+\-\*]                { \s -> operator (head s) }
    [\=\(\)]                { \s -> delimiter (head s) }
    let                     { \s -> TokenLet }
    in                      { \s -> TokenIn }
    end                     { \s -> TokenEnd }
    -- Added to support ifs:
    if                      { \s -> TokenIf }
    then                    { \s -> TokenThen }
    else                    { \s -> TokenElse }
    $alpha [$alphanum]*     { \s -> TokenVar s }

{

-- Helper functions:
operator :: Char -> Token
operator c = case c of '+' -> TokenAdd
                       '-' -> TokenSub
                       '*' -> TokenMul

delimiter :: Char -> Token
delimiter c = case c of '(' -> TokenLPar
                        ')' -> TokenRPar
                        '=' -> TokenEq

-- Enumeration of the unique possible tokens in our grammar.
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
            -- Added to support ifs:
            | TokenIf
            | TokenThen
            | TokenElse
            deriving (Show, Eq)

-- Interprets code in our language to a sequence of tokens.
exprLexer :: String -> [Token]
exprLexer = alexScanTokens
}