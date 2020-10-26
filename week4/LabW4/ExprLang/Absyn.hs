{- # Abstract syntax for the simple expression language # -}

module Absyn where

import           ExprLex

-- | Abstract Syntax Tree (AST) for the expression language.
data Expr = CstI Int
          | Var  String
          | Let  String Expr Expr
          | Prim String Expr Expr
          deriving Show
