module IfParse where

import           IfAbsyn
import           IfExprLex
import           IfExprPar

parseFromString :: String -> Expr
parseFromString = exprParser . exprLexer

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s

