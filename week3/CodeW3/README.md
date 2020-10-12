# Lexing and Parsing

## Lexing

We will be using the buildtool "Alex" to create lexers. You can use cabal to install
it:

```bash
$ cabal install alex
```

You can turn lexer specifications (in the form of `.x` files) into lexers by using
alex as a command line tool:

```bash
$ alex *.x
```

The above command will generate a haskell file of the same name. This is your lexer.

## Parsing

We will be using the buildtool "Happy" to create parsers. You can use cabal to install
it:

```bash
$ cabal install happy
```

You can turn parser specifications (in the form of `.y` files) into parsers by using
happy as a command line tool:

```bash
$ happy *.y
```

The above command will generate a haskell file of the same name. This is your parser.

You can also handwrite parsers. For examples, see `Handwritten.hs`.

## This Code

Do the following to try out the code from the lectures.

Generate your lexer:

```bash
$ alex ExprLex.x
```

You should now have a `ExprLex.hs` file.

Generate your parser:

```bash
$ happy ExprPar.y
```

You should now have a `ExprPar.hs` file.

Play with what you have generated:

```bash
ghci Parse.hs Absyn.hs ExprLex.hs ExprPar.hs
*Parse> parseFromString "x + 52 * wk"
```
