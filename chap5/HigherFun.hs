{- A functional language with integers and higher-order functions

   The language is higher-order because the value of an expression may
   be a function (and therefore a function can be passed as argument
   to another function).

   A function definition can have only one parameter, but a
   multiparameter (curried) function can be defined using nested
   function definitions:

      let f x = let g y = x + y in g end in f 6 7 end
-}

module HigherFun where

-- from chap4 FunLang
import Absyn

{- Environment operations -}

type Env a = [(String, a)]

envLookup :: Env a -> String -> a
envLookup env x =
    case env of
      []         -> error (x ++ " not found")
      ((y, v):r) -> if x == y then v else envLookup r x

{- A runtime value is an integer or a function closure -}

data Value = Num Int
           | Closure String String Expr (Env Value)       {- f x fBody fDeclEnv -}
        deriving (Show, Eq)

eval :: Expr -> Env Value -> Value
eval (CstI i) env = Num i
eval (CstB b) env = Num (if b then 1 else 0)
eval (Var  x) env = envLookup env x
eval (Prim ope e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (ope, v1, v2) of
        ("*", Num i1, Num i2) -> Num (i1 * i2)
        ("+", Num i1, Num i2) -> Num (i1 + i2)
        ("-", Num i1, Num i2) -> Num (i1 - i2)
        ("=", Num i1, Num i2) -> Num (if i1 == i2 then 1 else 0)
        ("<", Num i1, Num i2) -> Num (if i1 <  i2 then 1 else 0)
        _   -> error ("unknown primitive " ++ ope)
eval (Let x eRhs letBody) env =
    let xVal    = eval eRhs env
        bodyEnv = ((x, xVal):env)
    in  eval letBody bodyEnv
eval (If e1 e2 e3) env =
    case eval e1 env of
        Num 0 -> eval e3 env
        Num _ -> eval e2 env
        _     -> error "eval If"
eval (Letfun f x fBody letBody) env =
    let bodyEnv = ((f, Closure f x fBody env) : env)
    in  eval letBody bodyEnv
eval (Call eFun eArg) env =
    let fClosure = eval eFun env
    in  case fClosure of
         Closure f x fBody fDeclEnv ->
            let xVal = eval eArg env
                fBodyEnv = ((x, xVal):(f, fClosure):fDeclEnv)
            in  eval fBody fBodyEnv
         _ -> error "eval Call: not a function"

{- Evaluate in empty environment: program must have no free variables: -}

run e = eval e []

{- Examples in abstract syntax -}

ex1 = Letfun "f1" "x" (Prim "+" (Var "x") (CstI 1))
             (Call (Var "f1") (CstI 12))

{- Factorial -}

ex2 = Letfun "fac" "x"
                 (If (Prim "=" (Var "x") (CstI 0))
                     (CstI 1)
                     (Prim "*" (Var "x")
                               (Call (Var "fac")
                                     (Prim "-" (Var "x") (CstI 1)))))
                 (Call (Var "fac") (Var "n"))

{- fac10 = eval ex2 [("n", Int 10)] -}

ex3 =
    Letfun "tw" "g"
           (Letfun "app" "x" (Call (Var "g") (Call (Var "g") (Var "x")))
                  (Var "app"))
           (Letfun "mul3" "y" (Prim "*" (CstI 3) (Var "y"))
                  (Call (Call (Var "tw") (Var "mul3")) (CstI 11)))

ex4 =
    Letfun "tw" "g"
           (Letfun "app" "x" (Call (Var "g") (Call (Var "g") (Var "x")))
                  (Var "app"))
           (Letfun "mul3" "y" (Prim "*" (CstI 3) (Var "y"))
                  (Call (Var "tw") (Var "mul3")))
