module Parse where

-- local
import           Absyn
import           CLex
import           CPar

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
