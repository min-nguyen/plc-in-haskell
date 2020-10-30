{- This module contains two different evaluations of the Micro-ML language:
      - 'firstOrder', which evaluates it as a first-order language
      - 'higherOrder', which evaluates it as a higher-order language

There are example expressions for you to try out the different evaluators on at
the bottom.

-}

module Eval where

-- base
import           Prelude hiding (lookup)

-- local
import           Absyn

{- Environment operations -}

type Env a = [(String, a)]

lookup :: Env a -> String -> a
lookup env x =
    case env of
      []         -> error (x ++ " not found")
      ((y, v):r) -> if x == y then v else lookup r x

{- A runtime value is an integer or a function closure -}

data Value = Num Int
           | Closure String String Expr (Env Value) -- f x fBody fDeclEnv

{- First-order evaluation -}

-- | Evaluates a Micro-ML 'Expr' as a first-order language.
--   This means that a value cannot be a function.
firstOrder :: Expr -> Env Value -> Int
firstOrder (CstI i) env = i
firstOrder (CstB b) env = if b then 1 else 0
firstOrder (Var  x) env =
    -- Because this is the first-order evaluation, functions are not allowed as
    -- values, so we throw an error when anything but a 'Num' value is looked up.
    case lookup env x of
      Num i -> i
      _     -> error "firstOrder: functions cannot be values"
firstOrder (Prim ope e1 e2) env =
    let i1 = firstOrder e1 env
        i2 = firstOrder e2 env
    in case ope of
        "*" -> i1 * i2
        "+" -> i1 + i2
        "-" -> i1 - i2
        "=" -> if i1 == i2 then 1 else 0
        "<" -> if i1 < i2 then 1 else 0
        _   -> error ("firstOrder: unknown primitive " ++ ope)
firstOrder (Let x eRhs letBody) env =
    let xVal    = Num (firstOrder eRhs env)
        bodyEnv = ((x, xVal):env)
    in  firstOrder letBody bodyEnv
firstOrder (If e1 e2 e3) env =
    let b = firstOrder e1 env
    in  if b /= 0
        then firstOrder e2 env
        else firstOrder e3 env
firstOrder (Letfun f x fBody letBody) env =
    let bodyEnv = ((f, Closure f x fBody env) : env)
    in  firstOrder letBody bodyEnv
-- When evaluating as a first-order language, we assume calls are variables:
firstOrder (Call (Var f) eArg) env =
    let fClosure = lookup env f
    in  case fClosure of
         Closure f x fBody fDeclEnv ->
            let xVal = Num (firstOrder eArg env)
                fBodyEnv = ((x, xVal):(f, fClosure):fDeclEnv)
            in  firstOrder fBody fBodyEnv
         _ -> error "firstOrder Call: not a function"
firstOrder (Call _ _) env = error "firstOrder Call: not first-order function"

{- Higher-order evaluation -}

-- | Evaluates a Micro-ML 'Expr' as a first-order language.
--   There are subtle differences between this and 'firstOrder', which will be highlighted.
higherOrder :: Expr -> Env Value -> Value -- Now functions can be values, so the return type reflects that.
-- These cases are the same, except the return value is wrapped up with the 'Num' constructor.
higherOrder (CstI i) env = Num i
higherOrder (CstB b) env = Num (if b then 1 else 0)
-- Now functions are allowed as values, any lookup is valid, so no error checking is required
higherOrder (Var  x) env = lookup env x
-- This case requires the extra step of checking that the arguments evaluate to a 'Num'.
higherOrder (Prim ope e1 e2) env =
    -- Evaluate args:
    let v1 = higherOrder e1 env
        v2 = higherOrder e2 env
    -- Check both args have 'Num' constructor:
    in case (v1, v2) of
          -- When they do, evaluate operaton as before and wrap as a Vlaue using Num:
          (Num i1, Num i2) -> Num $
            case ope of
              "*" -> i1 * i2
              "+" -> i1 + i2
              "-" -> i1 - i2
              "=" -> if i1 == i2 then 1 else 0
              "<" -> if i1 < i2 then 1 else 0
              _   -> error ("higherOrder: unknown primitive " ++ ope)
          -- When they don't, throw an error:
          _ -> error "higherOrder: args of a primitive op did not evaluate to numbers."
-- The following cases are basically the same, bar the switch from Ints to Values
higherOrder (Let x eRhs letBody) env =
    let xVal    = higherOrder eRhs env
        bodyEnv = ((x, xVal):env)
    in  higherOrder letBody bodyEnv
higherOrder (If e1 e2 e3) env =
    case higherOrder e1 env of
        Num 0 -> higherOrder e3 env
        Num _ -> higherOrder e2 env
        _     -> error "higherOrder If"
higherOrder (Letfun f x fBody letBody) env =
    let bodyEnv = ((f, Closure f x fBody env) : env)
    in  higherOrder letBody bodyEnv
-- Finally, for the Call case, we run the function:
higherOrder (Call eFun eArg) env =
    let fClosure = higherOrder eFun env
    in  case fClosure of
         Closure f x fBody fDeclEnv ->
            let xVal = higherOrder eArg env
                fBodyEnv = ((x, xVal):(f, fClosure):fDeclEnv)
            in  higherOrder fBody fBodyEnv
         _ -> error "higherOrder Call: not a function"

{- Examples in abstract syntax -}

-- | Example:
ex1 = Letfun "f1" "x" (Prim "+" (Var "x") (CstI 1)) (Call (Var "f1") (CstI 12))

-- | Example: factorial
ex2 = Letfun "fac" "x" (If (Prim "=" (Var "x") (CstI 0))
                           (CstI 1)
                           (Prim "*" (Var "x") (Call (Var "fac") (Prim "-" (Var "x") (CstI 1)))))
                        (Call (Var "fac") (Var "n"))

-- | Example: deep recursion to check for constant-space tail recursion
ex3 = Letfun "deep" "x"
                 (If (Prim "=" (Var "x") (CstI 0))
                     (CstI 1)
                     (Call (Var "deep") (Prim "-" (Var "x") (CstI 1))))
                 (Call (Var "deep") (Var "count"))


-- | Example: static scope (result 14) or dynamic scope (result 25
ex4 =
    Let "y" (CstI 11)
        (Letfun "f" "x" (Prim "+" (Var "x") (Var "y"))
               (Let "y" (CstI 22) (Call (Var "f") (CstI 3))))

-- | Example: two function definitions: a comparison and Fibonacci
ex5 =
    Letfun "ge2" "x" (Prim "<" (CstI 1) (Var "x"))
           (Letfun "fib" "n"
                   (If (Call (Var "ge2") (Var "n"))
                       (Prim "+"
                             (Call (Var "fib") (Prim "-" (Var "n") (CstI 1)))
                             (Call (Var "fib") (Prim "-" (Var "n") (CstI 2))))
                       (CstI 1))
                   (Call (Var "fib") (CstI 25)))

