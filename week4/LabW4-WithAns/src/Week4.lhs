Week 4
================================================================================

> module Week4 where

> import Compiler
> import Parse

The main goal of this week's exercises is to familiarize yourself with regular
expressions, automata, grammars, the alex lexer generator and the happy parser
generator.

Exercise 1 - Regular Expressions and Finite Automata
--------------------------------------------------------------------------------

(i) Write a regular expression that recognizes all sequences consisting of "a"
    and "b" where two "a"'s are always separated by at least one "b". For instance,
    these four strings are legal: "b", "a",  "ba", "ababbbaba"; but these two
    strings are illegal: "aa", "babaa".

(ii) Construct the corresponding NFA.

Exercise 2 - Grammars
--------------------------------------------------------------------------------

(i) Write out the rightmost derivation of this string using the Expr grammar in
     ExprLang/ExprPar.y

< let z = 17 in z + 2 * 3 end

    (Take note of the grammar rules (1 to 8) used:)

(ii) Draw the above derivation as a tree.

Exercise 3 - Happy Alex
--------------------------------------------------------------------------------
For this lab, we have packaged up happy and alex into cabal for you. This means
that instead having to run them separately as command line processes, they will
automatically compile your .y and .x specifications into Haskell. (You can take
a look at the .cabal file to see what was added to do this).

(i) Have a play with the Parser provided in ExprLang by:
    1. Typing "cabal repl" on your command line.
    2. Loading in the parser code with ":l Parse"
    3. Using the 'parseFromString' function

For example,

< parseFromString "1 + 2 * 3"
< parseFromString "1 - 2 - 3"
< parseFromString "1 + -2"
< parseFromString "x++"
< parseFromString "1 + 1.2"
< parseFromString "1 + "
< parseFromString "let z = (17) in z + 2 * 3 end"
< parseFromString "let z = 17) in z + 2 * 3 end"
< parseFromString "let in = (17) in z + 2 * 3 end"
< parseFromString "1 + let x = 5 in let y = 7 + x in y + y end + x end"

Exercise 4 - Bringing it Together
--------------------------------------------------------------------------------
We will now combine the compiler we had last week with our new parser.

(i) Using the expression parser from ExprLang/Parse.hs, and the expression-to-stack-machine
    compiler 'scomp' and associated datatypes from ExprLang/Compiler.hs, define
    a function 'compString' that parses a string as an expression and compiles
    it to stack machine code.

> -- | Parses and compiles a string in the 'Expr' language to stack machine code.
> compString :: String -> [SInstr]
> compString s = undefined -- TASK: Define me!

(ii) Extend the expression language abstract syntax and the lexer and parser
     specifications with conditional expressions.

     The abstract syntax should be 'If e1 e2 e3'; so you need to modify file
    'ExprLang/Absyn.hs' as well as 'ExprLang/ExprLex.x' and 'ExprLang/ExprPar.y'.

    The concrete syntax should be in the style:

< if e1 then e2 else e3

(iii) (This exercise is optional and will require some self-study.)
      Determine the steps taken by the parser generated from 'ExprLang/ExprPar.y'
      during the parsing of this string:

< let z = 17 in z + 2 * 3 end

      For each step, show the remaining input, the parse stack, and the action
      (shift, reduce, or goto) performed. You will need a printout of the parser
      states and their transitions to do this exercise; to do this, use thw
      'parseFromString' function.

      Sanity check: the sequence of reduce action rule numbers in the parse should
      be the exact reverse of that found in the derivation in Ex2.

Exercise 5 - USQL
--------------------------------------------------------------------------------
Let's play with a bigger language!

Files in the directory 'UsqlLang/' include:
  UsqlAbsyn.hs - an abstract syntax
  UsqlLex.x    - a lexer specification
  UsqlPar.y    - a parser specification

All for micro-SQL, a small subset of the SQL database query language.

Extend micro-SQL to cover a larger class of sql select statements. Look at the
examples below and decide your level of ambition.

You should not need to modify file 'UsqlParse.hs'.
Don't forget to write some examples in concrete syntax to show that your parser
can parse them.

For instance, to permit an optional where clause, you may add one more component
to the Select constructor:

< data Stmt = Select [Expr]                 -- fields are expressions
<                    [String]               -- FROM ...
<                    (Maybe Expr)           -- optional WHERE clause

So that
  SELECT ... FROM ... WHERE ...
gives
  Select ... ... (Just ...)
and
  SELECT ... FROM ...
gives
  Select ... ... Nothing.

The argument to where is just an expression (which is likely to involve a comparison),
as in these examples:

  SELECT name, zip FROM Person WHERE income > 200000
  SELECT name, income FROM Person WHERE zip = 2300
  SELECT name, town FROM Person, Zip WHERE Person.zip = Zip.zip

More ambitiously, you may add optional group by and order by clauses in a similar way.
The arguments to these are lists of column names, as in this example:

  SELECT town, profession, AVG(income) FROM Person, Zip
  WHERE Person.zip = Zip.zip
  GROUP BY town, profession
  ORDER BY town, profession
