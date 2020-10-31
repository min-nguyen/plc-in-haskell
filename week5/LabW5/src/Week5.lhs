Week 5
================================================================================

> module Week5 where

> -- microML
> import Eval
> import Parse

The goal of this week's exercises is to understand:
  - The evaluation of a simple functional language
  - How explicit types can be given and checked
  - How polymorphism can be introduced
  - How types can be inferred

Exercise 1 - Micro-ML
--------------------------------------------------------------------------------
The 'microML' folder contains lexer and parser specifications and interpreters
for a small functional language. Just like the example code from week 4, two
interpreters are provided: one that interprets it as a first-order language, and
one that interprets it as a higher-order language.

(i) Get familiar with this language and the different evaluation methods by parsing
    and running some examples, which can be run easily in the "cabal repl":

> playFO = firstOrder (parseFromString "let f x = x + 7 in f 2 end") []
> playHO = higherOrder (parseFromString "let f x = x + 7 in f 2 end") []

(ii) Write more example programs in the functional language. Then test
     them in the same way as in (i).

     For instance, write programs that do the following:
      1. Compute the sum of the numbers from 1000 down to 1. Do this by defining
        a recursive function "sum n" that computes the sum "n+(n-1)+ ... + 2 + 1".
        (Use straightforward summation, no clever tricks).
      2. Compute the number "3^8", that is, 3 raised to the power 8.
         Again, use a recursive function.
      3. Compute "3^0 + 3^1 + ... + 3^{10} + 3^{11}", using two
        recursive functions.
      4. Compute "1^8 + 2^8 + ... + 10^8", again using two recursive
        functions.

(iii) For simplicity, the current implementation of the functional language requires
      all functions to take exactly one argument.  This seriously limits the programs
      that can be written in the language (at least it limits what that can be written
      without excessive cleverness and complications).

      Modify the language to permit functions to take one or more arguments by
      creating an alternative Absyn and first-order evaluator. (Don't override the
      old one because you will need it in ex4)

      (Don't bother making a lexer/parser for this, as that was covered last week.)

Exercise 2 - Explicit Types
--------------------------------------------------------------------------------
In the 'src' folder you will find 'ExplicitTypes', which is just the same as the
lecture provided code from last week, containing an explicitly typed strict first-order
functional language.

(i) Modify the abstract syntax 'TyExpr'/'Typ' and the type checker functions 'typ' and
   'typeCheck' in 'ExplicitTypes.hs' to allow functions to take any number of typed
    parameters.

    (Leave the evaluator as it is, as this is similar to the previous exercise.)

(ii) We will now expand our 'TyExpr' with homogeneous lists. You will need to add
     three constructors for this:
      * one for the empty list
      * one to represent cons
      * one to facilitate pattern matching on the list including:
          - the expression to match on (e0)
          - the expression that the empty list will be mapped to (e1)
          - the mapping of the cons case with names for head (h) and tail (t) of
            the list to an expression (e2)
        i.e. a pattern match in the language would look like:

        match e0 with [] -> e1 | h:t -> e2

    When you have added lists, extend the type checking to include them.

Exercise 3 - Polymorphism
--------------------------------------------------------------------------------

(i) What is the type of f in this micro-ML program?
    Is it polymorphic?
    Explain this needs to be the case.

< let f x = 1
< in f f end

(ii) What is the type of f in this micro-ML program?
     Is it polymorphic?
     Explain this needs to be the case.

< let f x = if x<10 then 42 else f(x+1)
< in f 20 end

Exercise 4 - Type Inference
--------------------------------------------------------------------------------

(i) Use the 'inferExpr' function in 'microML/TypeInference.hs' to type the following
    terms. Some of the type inferences will fail because the programs are
    not typable in micro-ML; in those cases, explain why the program is not typable:

> t1 = "let f x = 1 in f f end"
> t2 = "let f g = g g in f end"
> t3 = "let f x = let g y = y in g false end in f 42 end"
> t4 = "let f x = let g y = if true then y else x in g false end in f 42 end"
> t5 = "let f x = let g y = if true then y else x in g false end in f true end"

(ii) Write micro-ML programs for which the micro-ML type inference report the
     following types:

> prog1 = undefined -- TASK: define me to be a micro-ML program :: bool -> bool
> prog2 = undefined -- TASK: define me to be a micro-ML program :: int -> int
> prog3 = undefined -- TASK: define me to be a micro-ML program :: int -> int -> int
> prog4 = undefined -- TASK: define me to be a micro-ML program :: a -> b -> a
> prog5 = undefined -- TASK: define me to be a micro-ML program :: a -> b -> b
> prog6 = undefined -- TASK: define me to be a micro-ML program :: (a -> b) -> (b -> c) -> (a -> c)

Remember that the type arrow (->) is right associative, so int -> int -> int
is the same as int -> (int -> int), and that the choice of type variables
does not matter, so the type scheme h -> g -> h is the same as a ->
b -> a.
