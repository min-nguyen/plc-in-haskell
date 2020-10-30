module Answers5 where

-- base
import           Prelude hiding (lookup, all, exists, filter)

-- containers
import qualified Data.Map as M

-- microML
import           Eval
import qualified ExplicitTypes as ET
import           Parse
import           TypeInference (inferExpr, TypeEnv(..))
import qualified TypeInference as TI

-- Exercise 1 - Micro-ML
--------------------------------------------------------------------------------

-- (i)

-- $ cabal repl
-- *Week5> playFO
-- 9
-- *Week5> playHO
-- Num 9

-- (ii)

-- Two of the suggested functions:
-- (it might help to define them in Haskell first=, then translate)
sumHask 0 = 0
sumHask n = n + (sumHask (n-1))
f1 = "let sum n = if (n = 0) then 0 else n + sum (n - 1) in sum x end"

powHask 0 = 1
powHask n = 3 * (powHask (n-1))
f2 = "let pow n = if (n = 0) then 1 else 3 * pow (n - 1) in pow 8 end"
f3 = undefined
f4 = undefined

-- Evaluations:
-- FO:
evalf1FO = firstOrder (parseFromString f1) [("x", Num 9)]
evalf2FO = firstOrder (parseFromString f2) []
-- HO:
evalf1HO = higherOrder (parseFromString f1) []
evalf2HO = higherOrder (parseFromString f2) []

-- (iii)

data Expr' = CstI' Int
          | CstB' Bool
          | Var' String
          | Let' String Expr' Expr'
          | Prim' String Expr' Expr'
          | If' Expr' Expr' Expr'
          | Letfun' String [String] Expr' Expr'  -- f xs fBody letBody
          | Call' Expr' [Expr']
          deriving (Show, Eq)

data Value' = Num' Int
           | Closure' String [String] Expr' (Env Value') -- f xs fBody fDeclEnv
           deriving Show

firstOrder' :: Expr' -> Env Value' -> Int
firstOrder' (CstI' i) env = i
firstOrder' (CstB' b) env = if b then 1 else 0
firstOrder' (Var'  x) env =
    case lookup env x of
      Num' i -> i
      _     -> error "firstOrder': functions cannot be values"
firstOrder' (Prim' ope e1 e2) env =
    let i1 = firstOrder' e1 env
        i2 = firstOrder' e2 env
    in case ope of
        "*" -> i1 * i2
        "+" -> i1 + i2
        "-" -> i1 - i2
        "=" -> if i1 == i2 then 1 else 0
        "<" -> if i1 < i2 then 1 else 0
        _   -> error ("firstOrder': unknown primitive " ++ ope)
firstOrder' (Let' x eRhs letBody) env =
    let xVal    = Num' (firstOrder' eRhs env)
        bodyEnv = ((x, xVal):env)
    in  firstOrder' letBody bodyEnv
firstOrder' (If' e1 e2 e3) env =
    let b = firstOrder' e1 env
    in  if b /= 0
        then firstOrder' e2 env
        else firstOrder' e3 env
firstOrder' (Letfun' f xs fBody letBody) env =
    let bodyEnv = ((f, Closure' f xs fBody env) : env)
    in firstOrder' letBody bodyEnv
firstOrder' (Call' (Var' f) eArgs) env =
    let fClosure = lookup env f
    in case fClosure of
        Closure' f xs fBody fDeclEnv ->
           let xVals = map (Num' . flip firstOrder' env) eArgs
               varArgs = zip xs xVals
               fBodyEnv = ((f, fClosure): (varArgs ++ fDeclEnv))
           in firstOrder' fBody fBodyEnv
        _ -> error "firstOrder' Call: not a function"
firstOrder' (Call' _ _) env = error "firstOrder' Call: not first-order function"

-- Exercise 2 - Explicit Types
--------------------------------------------------------------------------------

-- (i)

data Typ = TypI                                {- int                         -}
         | TypB                                {- bool                        -}
         | TypF [Typ] Typ                      {- argumentTypes resultType    -}
        deriving (Show, Eq)

data TyExpr = CstI Int
            | CstB Bool
            | Var  String
            | Let  String TyExpr TyExpr
            | Prim String TyExpr TyExpr
            | If   TyExpr TyExpr TyExpr
            | Letfun String [(String, Typ)] TyExpr Typ TyExpr
            | Call  TyExpr [TyExpr]
            deriving Show

typ :: TyExpr -> Env Typ -> Typ
typ (CstI i) env = TypI
typ (CstB b) env = TypB
typ (Var  x) env = lookup env x
typ (Prim ope e1 e2) env =
    let t1 = typ e1 env
        t2 = typ e2 env
    in case (ope, t1, t2) of
        ("*", TypI, TypI) -> TypI
        ("+", TypI, TypI) -> TypI
        ("-", TypI, TypI) -> TypI
        ("=", TypI, TypI) -> TypB
        ("<", TypI, TypI) -> TypB
        ("&", TypB, TypB) -> TypB
        _                 -> error "unknown op, or type error"
typ (Let x eRhs letBody) env =
    let xTyp        = typ eRhs env
        letBodyEnv  = ((x, xTyp):env)
    in  typ letBody letBodyEnv
typ (If e1 e2 e3) env =
    case typ e1 env of
        TypB -> let t2 = typ e2 env
                    t3 = typ e3 env
                in  if   t2 == t3
                    then t2
                    else error "If: branch types differ"
        _    -> error "If: condition not boolean"
typ (Letfun f argTys fBody rTyp letBody) env =
    let fTyp       = TypF (snd <$> argTys) rTyp
        fBodyEnv   = argTys ++ ((f, fTyp):env)
        letBodyEnv = ((f, fTyp):env)
    in  if   typ fBody fBodyEnv == rTyp
        then typ letBody letBodyEnv
        else error ("Letfun: return type in" ++ f)
typ (Call (Var f) eArg) env =
    case lookup env f of
        TypF xsTyp rTyp -> if   (flip typ env <$> eArg) == xsTyp
                          then rTyp
                          else error "Call: wrong argument type"
        _              -> error "Call: unknown function"

typeCheck e = typ e []

-- (ii)

data LTyp = LTypI                                {- int                         -}
          | LTypB                                {- bool                        -}
          | LTypF [LTyp] LTyp                    {- argumentTypes resultType    -}
          -- Added to support lists:
          | TypL LTyp
          | TypNil
          deriving (Show, Eq)

data LTyExpr = LCstI Int
             | LCstB Bool
             | LVar  String
             | LLet  String LTyExpr LTyExpr
             | LPrim String LTyExpr LTyExpr
             | LIf   LTyExpr LTyExpr LTyExpr
             | LLetfun String [(String, LTyp)] LTyExpr LTyp LTyExpr
             | LCall  LTyExpr [LTyExpr]
             -- Added to support lists:
             | CstN
             | ConC LTyExpr LTyExpr
             | Match LTyExpr LTyExpr (String, String, LTyExpr)
             deriving Show

ltyp :: LTyExpr -> Env LTyp -> LTyp
ltyp (LCstI i) env = LTypI
ltyp (LCstB b) env = LTypB
ltyp (LVar  x) env = lookup env x
ltyp (LPrim ope e1 e2) env =
    let t1 = ltyp e1 env
        t2 = ltyp e2 env
    in case (ope, t1, t2) of
        ("*", LTypI, LTypI) -> LTypI
        ("+", LTypI, LTypI) -> LTypI
        ("-", LTypI, LTypI) -> LTypI
        ("=", LTypI, LTypI) -> LTypB
        ("<", LTypI, LTypI) -> LTypB
        ("&", LTypB, LTypB) -> LTypB
        _                 -> error "unknown op, or type error"
ltyp (LLet x eRhs letBody) env =
    let xLTyp        = ltyp eRhs env
        letBodyEnv  = ((x, xLTyp):env)
    in  ltyp letBody letBodyEnv
ltyp (LIf e1 e2 e3) env =
    case ltyp e1 env of
        LTypB -> let t2 = ltyp e2 env
                     t3 = ltyp e3 env
                in  if   t2 == t3
                    then t2
                    else error "If: branch types differ"
        _    -> error "If: condition not boolean"
ltyp (LLetfun f argLTys fBody rLTyp letBody) env =
    let fLTyp       = LTypF (snd <$> argLTys) rLTyp
        fBodyEnv   = argLTys ++ ((f, fLTyp):env)
        letBodyEnv = ((f, fLTyp):env)
    in  if   ltyp fBody fBodyEnv == rLTyp
        then ltyp letBody letBodyEnv
        else error ("Letfun: return type in" ++ f)
ltyp (LCall (LVar f) eArg) env =
    case lookup env f of
        LTypF xsLTyp rLTyp -> if   (flip ltyp env <$> eArg) == xsLTyp
                          then rLTyp
                          else error "Call: wrong argument type"
        _              -> error "Call: unknown function"
-- Added to support lists:
ltyp CstN env = TypNil
ltyp (ConC x CstN) env =
    let t1 = ltyp x env
    in TypL t1
ltyp (ConC x xs) env =
    let t1 = ltyp x env
        t2 = ltyp xs env
    in if TypL t1 == t2
    then TypL t1
    else error "badly typed list"

ltypeCheck e = ltyp e []

-- Exercise 3 - Polymorphism
--------------------------------------------------------------------------------

-- (i)

{-

f :: a -> Int
This function is polymorphic in its argument.
We need the polymorphism to be able to type the let body.

-}

{-

f :: Int -> Int
This function is not polymorphic.
We rely on the fact that x is an Int in the body of the let.

-}

-- Exercise 4 - Type Inference
--------------------------------------------------------------------------------

-- (i)

t1 = "let f x = 1 in f f end"
t2 = "let f g = g g in f end"
t3 = "let f x = let g y = y in g false end in f 42 end"
t4 = "let f x = let g y = if true then y else x in g false end in f 42 end"
t5 = "let f x = let g y = if true then y else x in g false end in f true end"

t1Ty = inferExpr (TypeEnv M.empty) $ parseFromString t1
t2Ty = inferExpr (TypeEnv M.empty) $ parseFromString t2
-- ^ ERRORS because we do not generalise type variables that are function params.
--   This is the first restriction on types that can be generalised that ensures
--   inference is implementable.
t3Ty = inferExpr (TypeEnv M.empty) $ parseFromString t3
t4Ty = inferExpr (TypeEnv M.empty) $ parseFromString t4
-- ^ ERRORS because g returns x or y, which have been given different types (Bool and Int)
--   So we do not know the type of g
t5Ty = inferExpr (TypeEnv M.empty) $ parseFromString t5

-- (ii)

-- Programs:
prog1 = "let f b = if b then false else true in f end"
prog2 = "let f x = x + 1 in f end"
prog3 = "let f x = let g y = x + y in g end in f end"
prog4 = "let f x = let g y = x in g end in f end"
prog5 = "let f x = let g y = y in g end in f end"
prog6 = "let comp f = let ose g = let arg x = g (f x) in arg end in ose end in comp end"

-- Types:
prog1Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog1
prog2Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog2
prog3Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog3
prog4Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog4
prog5Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog5
prog6Ty = inferExpr (TypeEnv M.empty) $ parseFromString prog6
