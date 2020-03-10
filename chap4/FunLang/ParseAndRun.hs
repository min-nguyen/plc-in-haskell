
module ParseAndRun where

import qualified Parse 
import qualified Fun 

-- | > ghci ParseAndRun.hs Fun.hs Absyn.hs Parse.hs FunLex.hs FunPar.hs

fromString = Parse.parseFromString

eval = Fun.eval

run e = eval e []

runProgram :: String -> Int
runProgram program = let e = fromString program
                     in  run e