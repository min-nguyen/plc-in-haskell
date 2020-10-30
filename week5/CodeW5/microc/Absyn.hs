module Absyn where

data Typ =
      TypI
    | TypC
    | TypA Typ (Maybe Int)
    | TypP Typ
    deriving Show

data Expr =
      Access  Access
    | Assign  Access Expr
    | Addr    Access
    | CstI    Int
    | Prim1   String Expr
    | Prim2   String Expr  Expr
    | AndAlso Expr   Expr
    | OrElse  Expr   Expr
    | Call    String [Expr]
    deriving Show

data Access =
      AccVar   String
    | AccDeref Expr
    | AccIndex Access Expr
    deriving Show

data Stmt =
      If     Expr Stmt Stmt
    | While  Expr Stmt
    | Expr   Expr
    | Return (Maybe Expr)
    | Block  [StmtOrDec]
    deriving Show

data StmtOrDec =
      Dec  Typ String
    | Stmt Stmt
    deriving Show

data TopDec =
      FunDec (Maybe Typ) String [(Typ, String)] Stmt
    | VarDec Typ String
    deriving Show

data Program =
      Prog [TopDec]
    deriving Show