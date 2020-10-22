{- # Abstract syntax for the simple expression language # -}

module IfAbsyn where

import           IfExprLex

-- | Abstract Syntax Tree (AST) for the expression language.
data Expr = CstI Int
          | Var  String
          | Let  String Expr Expr
          | Prim String Expr Expr
          -- Added to support ifs:
          | If   Expr   Expr Expr
          deriving Show
