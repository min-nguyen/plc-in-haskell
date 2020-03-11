module Parse where

import           Absyn
import           UsqlLex
import           UsqlPar

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs UsqlLex.hs UsqlPar.hs

parseFromString :: String -> Stmt
parseFromString = usqlParser . usqlLexer

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s
