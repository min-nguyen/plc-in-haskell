module Compiler where

import           Prelude                   hiding (lookup)

-- -----------------------------------------------------------------------------
-- Expr
-- -----------------------------------------------------------------------------
-- Our basic expression language that supports variables and let bindings with
-- static scope.

-- | Abstract Syntax Tree (AST) for the expression language.
data Expr = CstI Int
          | Var  String
          | Let  String Expr Expr
          | Prim String Expr Expr
          deriving Show

-- | Evaluator for 'Expr'.
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

-- Helper functions:
lookup :: [(String, Int)] -> String -> Int
lookup env x =
    case env of []         -> error (x ++ " not found")
                ((y, v):r) -> if x == y then v else lookup r x

run :: Expr -> Int
run e = eval e []

-- -----------------------------------------------------------------------------
-- Numeric indices
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

-- -----------------------------------------------------------------------------
-- Stack Machine
-- -----------------------------------------------------------------------------

-- | Stack machine instructions. An expressions in postfix or reverse Polish form
--   is a list of stack machine instructions.
data RInstr = RCstI Int
            | RAdd
            | RSub
            | RMul
            | RDup
            | RSwap
            deriving Show

-- | A simple stack machine for evaluation of variable-free expressions in postfix form
reval :: [RInstr] -> [Int] -> Int
reval [] (v:vs)                 = v
reval [] []                     = error "reval: no result on stack!"
reval ((RCstI i):insr) stk      = reval insr (i:stk)
reval (RAdd:insr) (i2:i1:stkr)  = reval insr ((i1+i2):stkr)
reval (RSub:insr) (i2:i1:stkr)  = reval insr ((i1-i2):stkr)
reval (RMul:insr) (i2:i1:stkr)  = reval insr ((i1*i2):stkr)
reval (RDup:insr) (i1:stkr)     = reval insr (i1:i1:stkr)
reval (RSwap:insr) (i2:i1:stkr) = reval insr (i1:i2:stkr)
reval _ _                       = error "reval: too few operands on stack"

-- | Compilation of a variable-free expression to a RInstr list
rcomp :: Expr -> [RInstr]
rcomp (CstI i)        = [RCstI i]
rcomp (Var _)         = error "rcomp cannot compile Var"
rcomp (Let _ _ _)     = error "rcomp cannot compile Let"
rcomp (Prim op e1 e2) =
    case op of
        "+" -> rcomp e1 ++ rcomp e2 ++ [RAdd]
        "-" -> rcomp e1 ++ rcomp e2 ++ [RSub]
        "*" -> rcomp e1 ++ rcomp e2 ++ [RMul]
        _   -> error "unknown primitive"

-- Correctness: eval e []  equals  reval (rcomp e) []


-- | Storing intermediate results and variable bindings in the same stack
data SInstr = SCstI Int     -- push integer
            | SVar Int      -- push variable from env
            | SAdd          -- pop args, push sum
            | SSub          -- pop args, push difference
            | SMul          -- pop args, push product
            | SPop          -- pop value/unbind var
            | SSwap         -- exchange top and next
   deriving Show

seval :: [SInstr] -> [Int] -> Int
seval [] (v:vs)                 = v
seval [] []                     = error "seval: no result on stack!"
seval ((SCstI i):insr) stk      = seval insr (i:stk)
seval ((SVar i):insr)  stk      = seval insr ((stk !! i):stk)
seval (SAdd:insr)  (i2:i1:stkr) = seval insr ((i1+i2):stkr)
seval (SSub:insr)  (i2:i1:stkr) = seval insr ((i1-i2):stkr)
seval (SMul:insr)  (i2:i1:stkr) = seval insr ((i1*i2):stkr)
seval (SPop:insr)  (_:stkr)     = seval insr stkr
seval (SSwap:insr) (i2:i1:stkr) = seval insr (i1:i2:stkr)
seval _ _                       = error "seval: too few operands on stack"


-- | A compile-time variable environment representing the state of
--   the run-time stack.
data StackValue = Value         -- a computed value
                | Bound String  -- a bound variable
                deriving Eq

-- | Compilation to a list of instructions for a unified-stack machine
scomp :: Expr -> [StackValue] -> [SInstr]
scomp (CstI i) cenv = [SCstI i]
scomp (Var x) cenv  = [SVar (getindex cenv (Bound x))]
scomp (Let x erhs ebody) cenv
    = scomp erhs cenv ++ scomp ebody (Bound x : cenv) ++ [SSwap, SPop]
scomp (Prim op e1 e2) cenv
    = case op of
        "+" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SAdd]
        "-" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SSub]
        "*" -> scomp e1 cenv ++ scomp e2 (Value:cenv) ++ [SMul]
        _   -> error "scomp: unknown operator"

-- Some closed expressions:
e1 = Let "z" (CstI 17) (Prim "+" (Var "z") (Var "z"))
e2 = Let "z" (CstI 17)
          (Prim "+" (Let "z" (CstI 22) (Prim "*" (CstI 100) (Var "z")))
                    (Var "z"))
e3 = Let "z" (Prim "-" (CstI 5) (CstI 4))
          (Prim "*" (CstI 100) (Var "z"))
e4 = Prim "+" (Prim "+" (CstI 20) (Let "z" (CstI 17)
                                       (Prim "+" (Var "z") (CstI 2))))
              (CstI 30)
e5 = Prim "*" (CstI 2) (Let "x" (CstI 3) (Prim "+" (Var "x") (CstI 4)))

-- Converted expressions:
s1 = scomp e1 []
s2 = scomp e2 []
s3 = scomp e3 []
s5 = scomp e5 []
