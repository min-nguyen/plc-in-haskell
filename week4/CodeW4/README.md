# Micro-ML

This week's featured the micro-ML language, which is a
  - functional
  - first-order
  TODO

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