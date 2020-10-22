module Parse where

import           Absyn
import           ExprLex
import           ExprPar

parseFromString :: String -> Expr
parseFromString = exprParser . exprLexer

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s

