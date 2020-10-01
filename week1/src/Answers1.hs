module Answers1 where

-- base
import           Prelude   hiding (lookup)

-- local
import           Intro     hiding (AExpr)

-- Exercise 1 - Haskell Recap
--------------------------------------------------------------------------------

max2 :: Int -> Int -> Int
max2 x y = if x > y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

isPositive :: [Int] -> Bool
isPositive xs = foldr (\x b -> x > 0 && b) True xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:ys) = x <= y && isSorted (y:ys)

data IntTree = Br Int IntTree IntTree | Lf

count :: IntTree -> Int
count (Br x l r) = 1 + count l + count r
count Lf = 0

depth :: IntTree -> Int
depth (Br x l r) = 1 + max2 (depth l) (depth r)

data Tree a = Node a (Tree a) (Tree a) | Leaf

linear :: Int -> Tree Int
linear n = if n == 0
          then Leaf
          else Node n Leaf (linear (n-1))

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: Tree a -> [a]
postorder Leaf = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

-- Exercise 2 - Expression Language
--------------------------------------------------------------------------------

-- (i)

evalQi :: Expr -> [(String, Int)] -> Int
evalQi (CstI i) env   = i
evalQi (Var x)  env   = lookup env x
evalQi (Prim op e1 e2) env
    | op == "+" = evalQi e1 env + evalQi e2 env
    | op == "*" = evalQi e1 env * evalQi e2 env
    | op == "-" = evalQi e1 env - evalQi e2 env
    | op == "max" = let e1' = evalQi e1 env
                        e2' = evalQi e2 env
                    in if e1' > e2' then e1' else e2'
    | op == "min" = let e1' = evalQi e1 env
                        e2' = evalQi e2 env
                    in if e1' < e2' then e1' else e2'
    | op == "=="  = let e1' = evalQi e1 env
                        e2' = evalQi e2 env
                    in if e1' == e2' then 1 else 0
    | otherwise = error "unknown primitive"

-- (ii)

exampleExpression1' = Prim "max" (CstI 4) (Var "b")
exampleExpression2' = Prim "==" (CstI 3) (Var "a")
exampleExpression3' = Prim "min" (Prim "*" (Var "b") (CstI 9)) (Var "a")

exampleExpression1v' = eval exampleExpression1' env
exampleExpression2v' = eval exampleExpression2' env
exampleExpression3v' = eval exampleExpression3' env

-- (iii)

evalQiii :: Expr -> [(String, Int)] -> Int
evalQiii (CstI i) env   = i
evalQiii (Var x)  env   = lookup env x
evalQiii (Prim op e1 e2) env
    = let i1 = evalQiii e1 env
          i2 = evalQiii e2 env
      in case op of
             "+" -> i1 + i2
             "*" -> i1 * i2
             "-" -> i1 - i2
             _   -> error "unknown primitive"

-- (iv)

data ExprQiv = CstIQiv Int
          | VarQiv String
          | PrimQiv String ExprQiv ExprQiv
          | If ExprQiv ExprQiv ExprQiv
          deriving Show

-- (v)

evalQv :: ExprQiv -> [(String, Int)] -> Int
evalQv (CstIQiv i) env   = i
evalQv (VarQiv x)  env   = lookup env x
evalQv (PrimQiv op e1 e2) env
    = let i1 = evalQv e1 env
          i2 = evalQv e2 env
      in case op of
             "+" -> i1 + i2
             "*" -> i1 * i2
             "-" -> i1 - i2
             _   -> error "unknown primitive"
evalQv (If e1 e2 e3) env
    = if evalQv e1 env /= 0
      then evalQv e2 env
      else evalQv e3 env

-- Exercise 3 - Arithmetic Expressions
--------------------------------------------------------------------------------

-- (i)

data AExpr = ACstI Int
           | AVar String
           | AAdd AExpr AExpr
           | AMul AExpr AExpr
           | ASub AExpr AExpr
           deriving (Show, Eq)

-- (ii)

aExpr1 = ASub (AVar "v") (AAdd (AVar "w") (AVar "z"))
aExpr2 = AMul (ACstI 2) (ASub (AVar "v") (AAdd (AVar "w") (AVar "z")))
aExpr3 = AAdd (AAdd (AAdd (AVar "x") (AVar "v")) (AVar "w")) (AVar "z")

-- (iii)

fmt :: AExpr -> String
fmt (ACstI i) = show i
fmt (AVar x)  = x
fmt (AAdd aexp1 aexp2) = "(" ++ fmt aexp1 ++ " + " ++ fmt aexp2 ++ ")"
fmt (AMul aexp1 aexp2) = "(" ++ fmt aexp1 ++ " * " ++ fmt aexp2 ++ ")"
fmt (ASub aexp1 aexp2) = "(" ++ fmt aexp1 ++ " - " ++ fmt aexp2 ++ ")"

-- (iv)

simplify :: AExpr -> AExpr
simplify (ACstI i) = ACstI i
simplify (AVar x)  = AVar x
simplify (AAdd aexp1 aexp2)
    = let aexp1' = simplify aexp1
          aexp2' = simplify aexp2
      in  case (aexp1', aexp2') of (ACstI 0, _) -> aexp2'
                                   (_, ACstI 0) -> aexp1'
                                   _            -> AAdd aexp1' aexp2'
simplify (ASub aexp1 aexp2)
    = let aexp1' = simplify aexp1
          aexp2' = simplify aexp2
      in  if aexp1' == aexp2'
          then ACstI 0
          else case aexp2' of (ACstI 0) -> aexp1'
                              _         -> ASub aexp1' aexp2'
simplify (AMul aexp1 aexp2)
    = let aexp1' = simplify aexp1
          aexp2' = simplify aexp2
      in  case (aexp1', aexp2') of (ACstI 0, _) -> aexp1'
                                   (_, ACstI 0) -> aexp2'
                                   (ACstI 1, _) -> aexp2'
                                   (_, ACstI 1) -> aexp1'
                                   _           -> AMul aexp1' aexp2'

-- (v)

aEval :: AExpr -> [(String, Int)] -> Int
aEval (ACstI i) env   = i
aEval (AVar x)  env   = lookup env x
aEval (AAdd aexp1 aexp2) env
  = let i1 = aEval aexp1 env
        i2 = aEval aexp2 env
    in i1 + i2
aEval (ASub aexp1 aexp2) env
  = let i1 = aEval aexp1 env
        i2 = aEval aexp2 env
    in i1 - i2
aEval (AMul aexp1 aexp2) env
  = let i1 = aEval aexp1 env
        i2 = aEval aexp2 env
    in i1 * i2

-- Exercise 4 - Brackets
--------------------------------------------------------------------------------

fmt' :: AExpr -> String
fmt' e = fmt2 e (-1)

fmt2 :: AExpr -> Int -> String
fmt2 (ACstI i) ctx_prec = show i
fmt2 (AVar x) ctx_prec  = x
fmt2 (AAdd aexp1 aexp2) ctx_prec = parens ctx_prec 6 (fmt2 aexp1 5 ++ " + " ++ fmt2 aexp2 6)
fmt2 (ASub aexp1 aexp2) ctx_prec = parens ctx_prec 6 (fmt2 aexp1 5 ++ " - " ++ fmt2 aexp2 6)
fmt2 (AMul aexp1 aexp2) ctx_prec = parens ctx_prec 7 (fmt2 aexp1 6 ++ " * " ++ fmt2 aexp2 7)

parens :: Int -> Int -> String -> String
parens ctx_prec prec aexpr = if ctx_prec < prec
                             then aexpr
                           else "(" ++ aexpr ++ ")"

-- Exercise 5 - Variable Binding
--------------------------------------------------------------------------------

-- (i)

data AExpr' = ACstI' Int
            | AVar' String
            | Let [(String, AExpr')] AExpr'
            | AAdd' AExpr' AExpr'
            | AMul' AExpr' AExpr'
            | ASub' AExpr' AExpr'
            deriving (Show, Eq)

-- (ii)

aEval' :: AExpr' -> [(String, Int)] -> Int
aEval' (ACstI' i) env   = i
aEval' (AVar' x)  env   = lookup env x
aEval' (Let xs ebody) env
  = let env1 = foldl f env xs
    in  aEval' ebody env1
    where f env' (x, xexpr) = let xval = aEval' xexpr env'
                              in  ((x, xval):env')
aEval' (AAdd' aexp1 aexp2) env
  = let i1 = aEval' aexp1 env
        i2 = aEval' aexp2 env
    in i1 + i2
aEval' (ASub' aexp1 aexp2) env
  = let i1 = aEval' aexp1 env
        i2 = aEval' aexp2 env
    in i1 - i2
aEval' (AMul' aexp1 aexp2) env
  = let i1 = aEval' aexp1 env
        i2 = aEval' aexp2 env
    in i1 * i2

-- (iii)

-- Helper functions:
-- One could also import similar functions from Data.List

-- | Determines whether a string is a member of a list of strings.
mem :: String -> [String] -> Bool
mem x vs
    = case vs of []     -> False
                 (v:vr) -> x == v || mem x vr

-- | Performs set union on two lists of strings.
union :: ([String], [String]) -> [String]
union (xs, ys)
    = case xs of []     -> ys
                 (x:xr) -> if mem x ys
                           then union (xr, ys)
                           else x : union (xr, ys)

-- | Performs set intersection on two lists of strings.
minus :: ([String], [String]) -> [String]
minus (xs, ys)
    = case xs of []     -> []
                 (x:xr) -> if mem x ys
                           then minus (xr, ys)
                           else x : minus (xr, ys)

-- Free variables function:

freeVars :: AExpr' -> [String]
freeVars (ACstI' i) = []
freeVars (AVar' x)  = [x]
freeVars (Let xs ebody) = foldr f (freeVars ebody) xs
  where f (x, erhs) freeVars' = union (freeVars erhs, minus (freeVars', [x]))
freeVars (AAdd' e1 e2) = union (freeVars e1, freeVars e2)
freeVars (ASub' e1 e2) = union (freeVars e1, freeVars e2)
freeVars (AMul' e1 e2) = union (freeVars e1, freeVars e2)