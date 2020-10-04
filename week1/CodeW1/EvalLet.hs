{- Evaluating simple expressions with variables and let expressions. -}

module EvalLet where

import           Prelude hiding (lookup)

{- Association lists map object language variables to their values -}

env      = [("a", 3), ("c", 78), ("baf", 666), ("b", 111)]

emptyenv = []

lookup :: [(String, Int)] -> String -> Int
lookup [] x         = error (x ++ " not found")
lookup ((y, v):r) x = if x == y then v else lookup r x

cvalue = lookup env "c"

{- Object language expressions with variables and let statements -}

data Expr = CstI Int
          | Var String
          | Let String Expr Expr
          | Prim String Expr Expr

{- Evaluation within an environment -}

eval :: Expr -> [(String, Int)] -> Int
eval (CstI i) env = i
eval (Var x) env = lookup env x
eval (Let x erhs ebody) env
  = let xval = eval erhs env
        env1 = ((x, xval):env)
    in eval ebody env1
eval (Prim op e1 e2) env
  | op == "+" = eval e1 env + eval e2 env
  | op == "*" = eval e1 env * eval e2 env
  | op == "-" = eval e1 env - eval e2 env
  | otherwise = error "unknown primitive"
