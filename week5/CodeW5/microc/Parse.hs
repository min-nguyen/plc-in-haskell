module Parse where

import           Absyn
import           CLex
import           CPar

parseFromString :: String -> Program
parseFromString = cParser . cLexer

parseFromFile :: String -> IO Program
parseFromFile filename = do
    s <- readFile filename
    let program = parseFromString s
    print program
    return program

lexFromFile :: String -> IO [Token]
lexFromFile filename = do
    s <- readFile filename
    let tokenStream = cLexer s
    print tokenStream
    return tokenStream