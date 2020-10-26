# Micro-ML

This week we are looking at the functional language: Micro-ML.

This code includes the following components of the Micro-ML language:
  - Parsing, which is much the same as previous weeks.
  - Evaluation, which this time has two variations, depending if we want to interpret
    Micro-ML as first-order, or as higher order.
  - Type checking/inference, which is brand new this week!

## Parsing

This code packages up the micro-ML language into a cabal file including:
  - AST
  - Parser
  - Lexer

Just like the this week's lab, cabal is set up to automatically run happy and alex
for you. This means that instead having to run them separately as command line processes, they will automatically compile your .y and .x specifications into Haskell. (You can take
a look at the .cabal file to see what was added to do this).

You can play with the examples in Parse.hs by:
    1. Typing "cabal repl" on your command line.
    2. Loading in the parser code with ":l Parse".
    3. Running one of the examples.

## Evaluation

'Eval.hs' contains two evaluation functions: one that interprets Micro-ML as first-order,
and one that interprets it as higher-order, allowing you to compare and contrast
the different methods.

## Type Checking/Inference

'TypeChecking.hs' contains a self contained example of an explicitly typed strict
first-order functional language.

'TypeInference.hs' contains type inference for Micro-ML.
