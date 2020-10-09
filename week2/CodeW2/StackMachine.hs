{- # Stack Machines # -}

module StackMachine where

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

-- Adding in stack machine:
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

-- | Map variable name to variable index at compile-time.
getindex :: Eq a => [a] -> a -> Int
getindex []     x = error "Variable not found"
getindex (y:yr) x = if x == y then 0 else 1 + getindex yr x

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

-- | Output the integers in list inss to the text file called fname:

intsToFile :: [Int] -> String -> IO ()
intsToFile inss fname = do
                let text = unwords (map show inss)
                writeFile fname text

assemble :: [SInstr] -> [Int]
assemble [] = []
assemble (x:xs)
 = let xs' = assemble xs
    in case x of SCstI i -> 0 : i : xs'
                 SVar  i -> 1 : i : xs'
                 SAdd    -> 2 : xs'
                 SSub    -> 3 : xs'
                 SMul    -> 4 : xs'
                 SPop    -> 5 : xs'
                 SSwap   -> 6 : xs'

scompeval :: Expr -> [StackValue] -> IO ()
scompeval x = flip intsToFile "fname" . assemble . scomp x
