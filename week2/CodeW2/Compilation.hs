{- Compilation to target expressions with numerical indexes instead of symbolic
   variable names.
-}

module Compilation where

import           Prelude                   hiding (lookup)

-- What we have so far:
-- -----------------------------------------------------------------------------

-- | Our language so far:
data Expr = CstI Int
          | Var  String
          | Let  String Expr Expr
          | Prim String Expr Expr
          deriving Show

-- | Its evaluator:
eval :: Expr -> [(String, Int)] -> Int
eval (CstI i) env   = i
eval (Var x)  env   = lookup env x
eval (Let x erhs ebody) env
    = let xval = eval erhs env
          env1 = ((x, xval):env)
      in  eval ebody env1
eval (Prim op e1 e2) env
    | op == "+" = eval e1 env + eval e2 env
    | op == "*" = eval e1 env * eval e2 env
    | op == "-" = eval e1 env - eval e2 env
    | otherwise = error "unknown primitive"

-- | 'eval' helper function
lookup :: [(String, Int)] -> String -> Int
lookup env x =
    case env of []         -> error (x ++ " not found")
                ((y, v):r) -> if x == y then v else lookup r x

-- Adding in Compilation without variable names:
-- -----------------------------------------------------------------------------

-- | 'Expr' AST with variable names removed:
data TExpr = TCstI Int
           | TVar Int         -- LOOK I have no 'String' :0
           | TLet TExpr TExpr -- LOOK I have no 'String' :0
           | TPrim String TExpr TExpr
           deriving Show

-- | Map variable name to variable index at compile-time.
getindex :: Eq a => [a] -> a -> Int
getindex []     x = error "Variable not found"
getindex (y:yr) x = if x == y then 0 else 1 + getindex yr x

-- | Compiling from 'Expr' to 'TExpr'
tcomp :: Expr -> [String] -> TExpr
tcomp (CstI i) cenv = TCstI i
tcomp (Var x) cenv = TVar (getindex cenv x)
tcomp (Let x erhs ebody) cenv
    = let cenv1 = (x:cenv)
      in  TLet (tcomp erhs cenv) (tcomp ebody cenv1)
tcomp (Prim op e1 e2) cenv
    = TPrim op (tcomp e1 cenv) (tcomp e2 cenv)

-- | Evaluation of target expressions with variable indexes. The
--   run-time environment renv is a list of variable values (ints).
--
-- Correctness:
-- >>> eval e [] == teval (tcomp e []) []
teval :: TExpr -> [Int] -> Int
teval (TCstI i) renv   = i
teval (TVar n)  renv   = renv !! n
teval (TLet erhs ebody) renv
    = let xval  = teval erhs renv
          renv1 = (xval:renv)
      in  teval ebody renv1
teval (TPrim op e1 e2) renv
    | op == "+" = teval e1 renv + teval e2 renv
    | op == "*" = teval e1 renv * teval e2 renv
    | op == "-" = teval e1 renv - teval e2 renv
    | otherwise = error "unknown primitive"
