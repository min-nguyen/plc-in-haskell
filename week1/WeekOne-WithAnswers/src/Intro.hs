
{- Evaluating simple expressions with variables

This file contains a simple expression language that supports
    * numbers
    * variables
    * the primitive binary operations of
        - addition
        - multiplication
        - subtraction
    * an environment that support mapping of variables to values

The eval function evaluates expressions in this language in a given environment.
 -}

module Intro where

-- local
import           Prelude hiding (lookup)

{- Association lists map object language variables to their values -}

env      = [("a", 3), ("c", 78), ("baf", 666), ("b", 111)]

emptyenv = []

lookup :: [(String, Int)] -> String -> Int
lookup [] x         = error (x ++ " not found")
lookup ((y, v):r) x = if x == y then v else lookup r x

cvalue = lookup env "c"

{- Object language expressions with variables -}

-- | Abstract Syntax tree for our expression language expressed as a Haskell data
--   data type.
data Expr = CstI Int
          | Var String
          | Prim String Expr Expr
          deriving Show

e1 = CstI 17
e2 = Prim "+" (CstI 3) (Var "a")
e3 = Prim "+" (Prim "*" (Var "b") (CstI 9)) (Var "a")

data AExpr = AExpr -- TASK: Expand me!

{- Evaluation within an environment -}

eval :: Expr -> [(String, Int)] -> Int
eval (CstI i) env   = i
eval (Var x)  env   = lookup env x
eval (Prim op e1 e2) env
    | op == "+" = eval e1 env + eval e2 env
    | op == "*" = eval e1 env * eval e2 env
    | op == "-" = eval e1 env - eval e2 env
    | otherwise = error "unknown primitive"

e1v  = eval e1 env
e2v1 = eval e2 env
e2v2 = eval e2 [("a", 314)]
e3v  = eval e3 env
