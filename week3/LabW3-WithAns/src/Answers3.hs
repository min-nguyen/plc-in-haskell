module Answers3 where

import Compiler

-- Exercise 1 - Compilation Without Variable Names
--------------------------------------------------------------------------------

-- (i)

data ExprSeq = CstISeq Int
             | VarSeq String
             | LetSeq [(String, ExprSeq)] ExprSeq
             | PrimSeq String ExprSeq ExprSeq
             deriving Show

tcompSeq :: ExprSeq -> [String] -> TExpr
tcompSeq (CstISeq i) cenv = TCstI i
tcompSeq (VarSeq x) cenv = TVar (getindex cenv x)
tcompSeq (LetSeq xs ebody) cenv
    = case xs of
        []              -> tcompSeq ebody cenv
        ((x, erhs):xs') -> let cenv1 = (x:cenv)
                           in  TLet (tcompSeq erhs cenv) (tcompSeq (LetSeq xs' ebody) cenv1)
tcompSeq (PrimSeq op e1 e2) cenv
    = TPrim op (tcompSeq e1 cenv) (tcompSeq e2 cenv)

-- Exercise 2 - Stack Machines
--------------------------------------------------------------------------------

-- (i)

-- Expressions as abstract syntax:

-- "let z = 17 in z + z"
expr1 = Let "z" (CstI 17) (Prim "+" (Var "z") (Var "z"))
expr1Ans = scomp expr1 [] -- which is: [SCstI 17,SVar 0,SVar 1,SAdd,SSwap,SPop]

-- "20 + let z = 17 in z + 2 end + 30"
expr2 = Prim "+"
        (CstI 20)
        (Prim "+"
          (Let "z" (CstI 17) (Prim "+" (Var "z") (CstI 2)))
          (CstI 30)
        )
expr2Ans = scomp expr2 [] -- which is: [SCstI 20,SCstI 17,SVar 0,SCstI 2,SAdd,SSwap,SPop,SCstI 30,SAdd,SAdd]

-- (ii)

expr1EvalAns = seval expr1Ans [] -- which is: 34
expr2EvalAns = seval expr2Ans [] -- which is: 69

-- (iii)

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

-- (iv)

scompeval :: Expr -> [StackValue] -> [Int]
scompeval x = assemble . scomp x

-- (v)

intsToFile :: [Int] -> String -> IO ()
intsToFile inss fname = do
                let text = unwords (map show inss)
                writeFile fname text

scompeval' :: Expr -> [StackValue] -> IO ()
scompeval' x = flip intsToFile "fname" . assemble . scomp x