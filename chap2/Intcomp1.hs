{- Programming language concepts for software developers, 2012-02-17    -}

{- Evaluation, checking, and compilation of object language expressions -}
{- Stack machines for expression evaluation                             -} 

{- Object language expressions with variable bindings and nested scope  -}

{-# LANGUAGE
     DeriveFunctor #-}

module Intcomp1 where

import Prelude hiding (lookup)
import Control.Applicative 

{- # Expressions with Let-Bindings and Static Scope # -}

data Expr = CstI Int
          | Var  String
          | Let  (String, Expr, Expr)
          | Prim (String, Expr, Expr)
          deriving Show

eval :: Expr -> [(String, Int)] -> Int 
eval (CstI i) env   = i
eval (Var x)  env   = lookup env x
eval (Let (x, erhs, ebody)) env
    = let xval = eval erhs env 
          env1 = ((x, xval):env)
      in  eval ebody env1
eval (Prim (op, e1, e2)) env 
    | op == "+" = eval e1 env + eval e2 env 
    | op == "*" = eval e1 env * eval e2 env
    | op == "-" = eval e1 env - eval e2 env
    | otherwise = error "unknown primitive"

lookup :: [(String, Int)] -> String -> Int
lookup env x = 
    case env of []         -> error (x ++ " not found")
                ((y, v):r) -> if x == y then v else lookup r x

run :: Expr -> Int
run e = eval e []


{-*----------------------------------------------------------------------*-}

{- # Closedness # -}

-- mem = elem

mem :: String -> [String] -> Bool
mem x vs 
    = case vs of []     -> False
                 (v:vr) -> x == v || mem x vr

-- |  Checking whether an expression is closed. An expression is closed 
--    if no variable occurs free in the expression. The 'vs' is a list 
--    of the bound variables. 

closedin :: Expr -> [String] -> Bool
closedin (CstI i) vs = True
closedin (Var x) vs  = mem x vs 
closedin (Let (x, erhs, ebody)) vs 
    = let vs1 = (x:vs)
      in  closedin erhs vs && closedin ebody vs1
closedin (Prim (op, e1, e2)) vs
    = closedin e1 vs && closedin e2 vs

-- | An expression is closed if it is closed in the empty environment 

closed1 e = closedin e []

-- | Some closed expressions: 

e1 = Let ("z", CstI 17, Prim ("+", Var "z", Var "z"))

e2 = Let ("z", CstI 17, 
          Prim ("+", Let ("z", CstI 22, Prim ("*", CstI 100, Var "z")),
                     Var "z"))

e3 = Let ("z", Prim ("-", CstI 5, CstI 4), 
          Prim ("*", CstI 100, Var "z"))

e4 = Prim ("+", Prim ("+", CstI 20, Let ("z", CstI 17, 
                                         Prim ("+", Var "z", CstI 2))),
                CstI 30)

e5 = Prim ("*", CstI 2, Let ("x", CstI 3, Prim ("+", Var "x", CstI 4)))


{-*----------------------------------------------------------------------*-}

{- # The Set of Free Variables # -}


-- x occurs bound in the body of 'let x = 6 in x + 3'
-- x occurs free in 'let y = 6 in x + 3'
--                  'let y = x in y + 3'
-- x occurs free (the first time) and bound (the second time) in 'let x = x + 6 in x + 3'


-- | union(xs, ys) is the set of all elements in xs or ys, without duplicates

union :: ([String], [String]) -> [String]
union (xs, ys) 
    = case xs of []     -> ys 
                 (x:xr) -> if mem x ys 
                           then union (xr, ys) 
                           else (x:(union (xr, ys)))

-- | minus(xs, ys) is the set of all elements in xs but not in ys          

minus :: ([String], [String]) -> [String]
minus (xs, ys)
    = case xs of []     -> []
                 (x:xr) -> if mem x ys 
                           then minus (xr, ys)
                           else (x:(minus (xr, ys)))

-- | Find all variables that occur free in expression e

freevars :: Expr -> [String]
freevars (CstI i) = []
freevars (Var x)  = [x]
freevars (Let (x, erhs, ebody)) 
    = union (freevars erhs, minus (freevars ebody, [x]))
freevars (Prim (op, e1, e2)) 
    = union (freevars e1, freevars e2)

-- | Alternative definition of closed

closed2 e = (freevars e == [])


{-*----------------------------------------------------------------------*-}

{- # Substitution: Replacing Free Variables with Expressions # -}


-- | This version of lookup returns a Var(x) expression if there is no
--   pair (x,e) in the list env --- instead of failing with exception: 

lookOrSelf :: [(String, Expr)] -> String -> Expr 
lookOrSelf env x 
    = case env of []         -> Var x 
                  ((y, e):r) -> if x == y then e else lookOrSelf r x

-- | Remove (x, _) from env: 

remove :: [(String, Expr)] -> String -> [(String, Expr)]
remove env x 
    = case env of []         -> []
                  ((y, e):r) -> if x == y then r else ((y, e):(remove r x))


-- | Naive Substitution (may capture free variables) 

nsubst :: Expr -> [(String, Expr)] -> Expr 
nsubst (CstI i) env = CstI i
nsubst (Var x)  env = lookOrSelf env x 
nsubst (Let (x, erhs, ebody)) env
    = let newenv = remove env x 
      in  Let (x, nsubst erhs env, nsubst ebody newenv)
nsubst (Prim (op, e1, e2)) env
    = Prim (op, nsubst e1 env, nsubst e2 env)

-- Some expressions with free variables 
e6   = Prim ("+", Var "y", Var "z")

e6s1 = nsubst e6 [("z", CstI 17)]

e6s2 = nsubst e6 [("z", Prim ("-", CstI 5, CstI 4))]

e6s3 = nsubst e6 [("z", Prim ("+", Var "z", Var "z"))]

-- Shows that only z outside the Let gets substituted
e7   = Prim ("+", Let ("z", CstI 22, Prim ("*", CstI 5, Var "z")),
                  Var "z")

e7s1 = nsubst e7 [("z", CstI 100)]

-- Shows that only the z in the Let rhs gets substituted
e8   = Let ("z", Prim ("*", CstI 22, Var "z"), Prim ("*", CstI 5, Var "z"))

e8s1 = nsubst e8 [("z", CstI 100)]

-- Shows (wrong) capture of free variable z under the Let
e9   = Let ("z", CstI 22, Prim ("*", Var "y", Var "z"))

e9s1 = nsubst e9 [("y", Var "z")]

e9s2 = nsubst e9 [("z", Prim ("-", CstI 5, CstI 4))]


-- | Correct Substitution (avoids capturing free variables)

newtype Fresh s a = Fresh (s -> (a, s)) deriving Functor

instance Applicative (Fresh s) where
    pure a = Fresh (\s -> (a, s))
    (<*>) = liftA2 id

instance Monad (Fresh s) where 
    return x = Fresh $ \s -> (x, s)
    (Fresh h) >>= f = Fresh $ \s -> let (a, newState) = h s 
                                        (Fresh g) = f a 
                                    in  g newState

fresh :: Enum x => Fresh x x
fresh = Fresh (\x -> (x, succ x)) 

newVar :: String -> String 
newVar = let n = 0
             varMaker x = let n' = 1 + n 
                          in  x ++ show n'
         in  varMaker

subst :: Expr -> StateT [(String, Expr)] (Fresh Int) Expr 
subst (CstI i) = return (CstI i)
subst (Var x)
    = do 
     env' <- env  
                    lookOrSelf env x 
subst (Let (x, erhs, ebody)) env
    = let newx   = newVar x
          newenv = ((x, Var newx):(remove env x)) 
      in  Let (newx, subst erhs env, subst ebody newenv)
subst (Prim (op, e1, e2)) env
    = Prim (op, subst e1 env, subst e2 env)

-- Shows renaming of bound variable z (to z1)
e7s1a = subst e7 [("z", CstI 100)]

-- Shows renaming of bound variable z (to z2)
e8s1a = subst e8 [("z", CstI 100)]

-- Shows renaming of bound variable z (to z3), avoiding capture of free z
e9s1a = subst e9 [("y", Var "z")]

{-*----------------------------------------------------------------------*-}

-- | Compilation to target expressions with numerical indexes instead of
--   symbolic variable names.

data TExpr = TCstI Int 
           | TVar Int 
           | TLet (TExpr, TExpr)
           | TPrim (String, TExpr, TExpr)
           deriving Show 

-- | Map variable name to variable index at compile-time

getindex :: [String] -> String -> Int
getindex vs x 
    = case vs of []     -> error "Variable not found"
                 (y:yr) -> if x == y then 0 else 1 + getindex yr x

-- | Compiling from Expr to TExpr

tcomp :: Expr -> [String] -> TExpr 
tcomp (CstI i) cenv = TCstI i
tcomp (Var x) cenv = TVar (getindex cenv x)
tcomp (Let (x, erhs, ebody)) cenv
    = let cenv1 = (x:cenv)
      in  TLet (tcomp erhs cenv, tcomp ebody cenv1)
tcomp (Prim (op, e1, e2)) cenv
    = TPrim (op, tcomp e1 cenv, tcomp e2 cenv)

-- | Evaluation of target expressions with variable indexes. The
--   run-time environment renv is a list of variable values (ints).

teval :: TExpr -> [Int] -> Int 
teval (TCstI i) renv   = i
teval (TVar n)  renv   = renv !! n
teval (TLet (erhs, ebody)) renv
    = let xval  = teval erhs renv 
          renv1 = (xval:renv)
      in  teval ebody renv1
teval (TPrim (op, e1, e2)) renv 
    | op == "+" = teval e1 renv + teval e2 renv 
    | op == "*" = teval e1 renv * teval e2 renv
    | op == "-" = teval e1 renv - teval e2 renv
    | otherwise = error "unknown primitive"

-- | Correctness: eval e []  equals  teval (tcomp e []) [] 


{-*----------------------------------------------------------------------*-}

{- # Stack Machines # -}

-- | Stack machine instructions. An expressions in postfix or reverse
--   Polish form is a list of stack machine instructions.

data RInstr = RCstI Int 
            | RAdd 
            | RSub 
            | RMul
            | RDup
            | RSwap
            deriving Show 

-- | A simple stack machine for evaluation of variable-free expressions
--   in postfix form

