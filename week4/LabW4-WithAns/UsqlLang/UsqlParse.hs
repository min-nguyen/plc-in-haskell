module UsqlParse where

import           UsqlAbsyn
import           UsqlLex
import           UsqlPar

parseFromString :: String -> Stmt
parseFromString = usqlParser . usqlLexer

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s
