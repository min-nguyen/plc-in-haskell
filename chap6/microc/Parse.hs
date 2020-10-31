module Parse where

import           Absyn
import           CLex
import           CPar

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs CLex.hs CPar.hs

parseFromString :: String -> Program
parseFromString = cParser . cLexer

parseFromFile :: String -> IO ()
parseFromFile filename = do
    s <- readFile filename
    print $ parseFromString s

lexFromFile :: String -> IO ()
lexFromFile filename = do
    s <- readFile filename
    print $ cLexer s
