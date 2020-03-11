module Parse where

import           Absyn
import           ExprLex
import           ExprPar

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs ExprLex.hs ExprPar.hs

parseFromString :: String -> Expr
parseFromString s = (exprParser . exprLexer) s

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s

