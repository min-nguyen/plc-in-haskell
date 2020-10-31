Week 3
================================================================================

> module Week3 where

> -- base
> import           Data.List                 (intercalate)

> -- local
> import           Compiler

This week we will solidify your knowledge of compiling with indices instead of variable
names and using a stack machine. We will do this by editing and expanding the code
from the lectures.

Exercise 1 - Compilation Without Variable Names
--------------------------------------------------------------------------------
This exercise will familiarise you will compiling with indices instead of variables
by taking the 'TExpr' type introduced in lectures and expending it and its 'tcomp'
compilation function.

(i) In the last lab, we have extend the expression language 'Expr' from Intro.hs
    with multiple sequential let-bindings, such as this (in concrete syntax):

< let x1 = 5+7   x2 = x1*2 in x1+x2 end

> data ExprSeq = CstISeq Int
>              | VarSeq String
>              | LetSeq [(String, ExprSeq)] ExprSeq
>              | PrimSeq String ExprSeq ExprSeq
>              deriving Show

   Revise the 'Expr'-to-'TExpr' compiler 'tcomp :: Expr -> TExpr' from Compiler.hs
   to work for the extended 'ExprSeq' language.

> -- | Compiles the sequential let expression language to our internal 'TExpr' representation.
> tcompSeq :: ExprSeq -> [String] -> TExpr
> tcompSeq = undefined -- TASK: Define me!

Exercise 2 - Stack Machines
--------------------------------------------------------------------------------

(i) Get your head around compiling with a stack by using the 'scomp' compiler from
    'Compiler' to turn the following expressions into a sequence of instructions.

    Try the process out on paper first, then check your answer using GHCi.

    NOTE:- you will need to translate the programs into abstract syntax before calling 'scomp'.

    Expressions:

< let z = 17 in z + z
< 20 + let z = 17 in z + 2 end + 30

(ii) Now execute your instruction sequences according to 'seval' by specifying
     the stack content at each step.

     Again, work it out on paper before using GHCi.

     NOTE:- if your execution results in an error, it may mean that your instruction
            sequence from above is wrong.

You are strongly encouraged to repeat these exercises with other expressions you
may have come up with (checking the answers with GHCi).

(iii) Write a bytecode assembler (in Haskell) that translates a list of bytecode
      instructions for the simple stack machine in Compiler.hs into a list of integers.

      The integers should be the corresponding bytecodes as seen below.

      SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6

> -- | Translates stack instructions to bite code instructions.
> assemble :: [SInstr] -> [Int]
> assemble = undefined -- TASK: Define me!

(iv) Use this function together with 'scomp' from 'Compiler.hs' to make a compiler
     from the original expressions language 'Expr' to a list of bytecodes '[Int]'.

> scompeval :: Expr -> [StackValue] -> [Int]
> scompeval x = undefined -- TASK: Define me

(v) Modify the compiler from the previous question to write the lists of integers
    to a file.

    A list 'inss' of integers may be output to the file called 'fname' using this
    function (found in 'Compiler.hs'):

> intsToFile :: [Int] -> String -> IO ()
> intsToFile inss fname
>   = do
>      let text = intercalate " " (map show inss)
>      writeFile fname text

> scompeval' :: Expr -> [StackValue] -> IO ()
> scompeval' x = undefined -- TASK: Define me
