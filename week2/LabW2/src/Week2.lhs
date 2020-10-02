Week 2
================================================================================

> module Week2 where

> import Intro

This lab should reacquaint you with Haskell and show you how to create, interpret,
format, extend and evaluate a small expression language in Haskell that also supports
variables and bindings.

Exercise 1 - Haskell Recap
--------------------------------------------------------------------------------
The purpose of this exercise is to review Haskell programming by defining different
functions. The functions are grouped by what they recap so just do the ones you
would like extra practice of. If you are confident with Haskell, skip to the next
question.

Remember how to write functions in Haskell by defining the following functions:

> -- | Returns the largest of its two arguments.
> --
> -- Example:
> --
> -- >>> max2 99 3
> -- 3
> --
> max2 :: Int -> Int -> Int
> max2 = undefined -- TASK: Define me!

> -- | Returns the largest of its three integer arguments.
> max3 ::Int -> Int -> Int -> Int
> max3 = undefined -- TASK: Define me!

To remember how functions over lists work, define these:

> -- | True if all elements of the list are greater than 0, and False otherwise.
> isPositive :: [Int] -> Bool
> isPositive = undefined -- TASK: Define me!

> -- | True if the elements of the list appear in non-decreasing order, and false otherwise.
> --
> -- Examples:
> --
> -- >>> isSorted [11, 12, 12]
> -- True
> --
> -- >>> isSorted [12, 11, 12]
> -- False
> --
> -- NOTE:- lists that are singleton or empty (i.e. [1] or []) are trivially sorted.
> isSorted :: [Int] -> Bool
> isSorted = undefined -- TASK: Define me!

For practice on functions over custom data types try these:

> -- | Simple custom data type.
> data IntTree = Br Int IntTree IntTree | Lf

> -- | Counts the number of internal nodes ('Br' constructors) in an 'IntTree'.
> --
> -- Examples:
> --
> -- >>> count (Br 37 (Br 117 Lf Lf) (Br 42 Lf Lf))
> -- 3
> --
> -- >>> count Lf
> -- 0
> --
> count :: IntTree -> Int
> count = undefined -- TASK: Define me!

> -- | Measures the depth of an 'IntTree', that is, the maximal number of
> -- internal nodes ('Br' constructors) on a path from the root to a leaf.
> --
> -- Examples:
> --
> -- >>> depth (Br 37 (Br 117 Lf Lf) (Br 42 Lf Lf))
> -- 2
> --
> -- >>> depth Lf
> -- 0
> depth :: IntTree -> Int
> depth = undefined -- TASK: Define me!

For practice with polymorphic data types define these:

> -- | A polymorphic tree data type.
> data Tree a = Node a (Tree a) (Tree a) | Leaf

> -- | Creates a "right-linear" tree with 'n' nodes
> --   A "right-linear" tree is one that only grows down the right-hand child.
> --   The 'Int' dictates the depth of the tree, where each node's value is the
> --   depth of its right child.
> --
> -- Examples:
> -- >>> linear 0
> -- Lf
> --
> -- >>> linear 2
> -- Br 2 Lf (Br 1 Lf Lf)
> --
> linear :: Int -> Tree Int
> linear = undefined -- TASK: Define me!

> -- | The following functions transform trees into lists using varying orderings
> --   of root and subtrees.

> -- | Converts a 'Tree' to a list where the root comes before the left subtree,
> --   which comes before right subtree.
> preorder :: Tree a -> [a]
> preorder Leaf = []
> preorder (Node v t1 t2) = v : preorder t1 ++ preorder t2

> -- | Converts a 'Tree' to a list where the left subtree comes before the root,
> --   which comes before right subtree.
> inorder :: Tree a -> [a]
> inorder = undefined -- TASK: Define me!

> -- | Converts a 'Tree' to a list where the left subtree comes before right subtree,
> --   which comes before root.
> postorder :: Tree a -> [a]
> postorder = undefined -- TASK: Define me!

Exercise 2 - Expression Language
--------------------------------------------------------------------------------
This exercise will familiarise you with the small expression language, by asking
you to extend its different parts.

(i) Take a look at our import 'Intro.hs' and extend the 'eval' function to support
    three extra primitive binary operations:
      1) max
      2) min
      3) (==) - this operator should return 0 for False, and 1 for True

(ii) Write some example expressions in the language and evaluate them with the 'eval' function.

> -- Example Expressions:

> exampleExpression1 :: Expr
> exampleExpression1 = undefined -- TASK: Define me!

> exampleExpression2 :: Expr
> exampleExpression2 = undefined -- TASK: Define me!

> exampleExpression3 :: Expr
> exampleExpression3 = undefined -- TASK: Define me!

> -- Evaluated Example Expressions:

> exampleExpression1v :: Int
> exampleExpression1v = undefined -- TASK: Define me!

> exampleExpression2v :: Int
> exampleExpression2v = undefined -- TASK: Define me!

> exampleExpression3v :: Int
> exampleExpression3v = undefined -- TASK: Define me!

(iii) Rewrite the 'eval' function to evaluate the arguments of a primitive before
      branching out on the operator, in this style:

< eval :: Expr -> [(String, Int)] -> Int
< eval (CstI i) env   = ...
< eval (Var x)  env   = ...
< eval (Prim op e1 e2) env
<     = let i1 = ...
<           i2 = ...
<       in case op of
<              "+" -> i1 + i2
<              ...

(iv) Extend the expression language with conditional expressions 'If e1 e2 e3'
     corresponding to Haskell's conditional expression 'if e1 then e2 else e3'

    You need to extend the 'Expr' datatype with a new constructor 'If' that takes
    three \Expr' arguments.

(v) Extend the interpreter function 'eval' correspondingly.  It should evaluate
    'e1', and if 'e1' is non-zero, then evaluate 'e2', else evaluate 'e3'.

    You should be able to evaluate this expression 'If (Var "a") (CstI 11) (CstI 22)'
    in an environment that binds variable 'a'.

    Note that various strange and non-standard interpretations of the conditional
    expression are possible.  For instance, the interpreter might start by testing
    whether expressions 'e2' and 'e3' are syntactically identical, in which case
    there is no need to evaluate 'e1', only 'e2' (or 'e3').  Although possible,
    this is rarely useful.

Exercise 3 - Arithmetic Expressions
--------------------------------------------------------------------------------

(i) Expand the alternative datatype 'AExpr' for a representation of arithmetic
    expressions without the use of a 'String'. The datatype should have constructors
    'CstI', 'Var', 'Add', 'Mul', 'Sub', for constants, variables, addition, multiplication,
    and subtraction.

    The idea is that we can represent "x*(y+3)""

< Mul (Var "x") (Add (Var "y") (CstI 3))

    instead of

< Prim "*" (Var "x") (Prim "+" (Var "y") (CstI 3))

(ii) Write the representation of the following expressions:
        1) v-(w+z)
        2) 2*(v-(w+z))
        3) x+y+z+v

> aExpr1 = undefined -- TASK: Define me!
> aExpr2 = undefined -- TASK: Define me!
> aExpr3 = undefined -- TASK: Define me!

(iii) Implement the following Haskell function:

> -- | Formats arithmetic expressions into strings.
> --
> -- Example:
> --
> -- >>> Sub (Var "x") (CstI 34)
> -- "(x - 34)"
> --
> fmt :: AExpr -> String
> fmt = undefined -- TASK: Define me!

HINT:- it is similar to the 'eval' function, except that it doesn't take an environment.

(iv) Implement this Haskell function for simplifying an arithmetic expression:

> -- | Performs expression simplification using the following table:
> --
> --     0 + e --> e
> --     e + 0 --> e
> --     e - 0 --> e
> --     1 * e --> e
> --     e * 1 --> e
> --     0 * e --> 0
> --     e * 0 --> 0
> --     e - e --> 0
> --
> simplify :: AExpr -> AExpr
> simplify = undefined -- TASK: Define me!

If you are feeling ambitious, you can simplify things like "(1+0)*(x+0)" to "x".
Feel free to customise your simplifications.

HINT:- Pattern matching is your friend.
HINT:- Don't forget the case where you cannot simplify anything.

(v) Write an evaluator for this type.

> -- | Evaluates an 'AExpr'.
> aEval :: AExpr -> [(String, Int)] -> Int
> aEval = undefined -- TASK: Define me!

Exercise 4 - Brackets
--------------------------------------------------------------------------------
Improve your 'fmt' function so that it avoids producing excess parentheses.

Examples:
(see bracketing.jpg)

< expr = Mul (Sub (Var "a") (Var "b")) (Var "c")
< -- Over Bracketing:
< fmt expr = "((a-b)*c)"
< -- Limited Bracketing:
< fmt expr = "(a-b)*c"

< expr = Sub (Mul (Var "a") (Var "b")) (Var "c")
< -- Over Bracketing:
< fmt expr = "((a*b)-c)"
< -- Limited Bracketing:
< fmt expr = "a*b-c"

The bracket minimisation should take into account that our operators associate to the left.
Example:

< uselessBrackets = Sub (Sub (Var "a") (Var "b")) (Var "c")
< fmt uselessBrackets = "a-b-c"

< importantBrackets = Sub (Var "a") (Sub (Var "b") (Var "c"))
< fmt importantBrackets = "a-(b-c)"

< fmt expr = "a*b-c"

Hint:- This can be achieved by declaring the formatting function to
take an extra parameter 'pre' that indicates the precedence or
binding strength of the context.  The new formatting function then has
type 'fmt :: AExpr -> Int -> String'.

Higher precedence means stronger binding.  When the top-most operator
of an expression to be formatted has higher precedence than the
context, there is no need for parentheses around the expression.  A
left associative operator of precedence 6, such as minus ('-'),
provides context precedence 5 to its left argument, and context
precedence 6 to its right argument.

As a consequence, 'Sub (Var "a") (Sub (Var "b") (Var "c"))' will be
parenthesized 'a - (b - c)' but 'Sub (Sub (Var "a") (Var "b")) (Var "c")'
 will be parenthesized 'a - b - c'.

Exercise 5 - Variable Binding and Substitution
--------------------------------------------------------------------------------

(i) Extend 'AExpr' to allow for multiple sequential let-bindings, such as this
    (in concrete syntax):

< let x1 = 5+7 x2 = x1*2 in x1+x2 end

So that the 'Let' constructor takes a list of bindings, where a binding is a pair
of a variable name and an expression. The example above would be represented as:

< Let [("x1", ...), ("x2", ...)] (Add (Var "x1") (Var "x2"))

(ii) Revise your evaluator to work on this 'AExpr'.

(iii) Define a function that forms a list of the free variables in an 'AExpr'.

      A free variable is one that is not bound. In our little language, the only
      binder is "let", thus free variables are ones that are not named in a let
      clause. This means that the example expression in (i) has not free variables
      because both x1 and x2 are bound by the let.

> -- | Creates a list of the free variables in an 'AExpr'
> freeVars :: AExpr -> [String]
> freeVars = undefined -- TASK: Define me!
