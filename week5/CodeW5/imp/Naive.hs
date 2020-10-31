{-
   A naive imperative language with "for" and "while" loops

   To test this out and execute example programs, run:
   > ghci Naive.hs
   > run ex1
   > run ex2
-}


module Naive where

-- base
import           Data.Maybe

-- containers
import qualified Data.Map as Map
import           Data.Map (Map)

-- | A naive store is essentially an environment
--   i.e. a map from names (strings) to values (ints)
type NaiveStore = Map String Int

-- | Create empty store
emptyStore :: Map String Int
emptyStore = Map.empty

-- | Get value of variable from store
getStore :: NaiveStore -> String -> Int
getStore store s = case Map.lookup s store of
                     Nothing -> error $   "getStore failed - "
                                       ++ s
                                       ++ " not found in store"
                     Just i  -> i

-- | Set value of variable in store
setStore :: NaiveStore -> String -> Int -> NaiveStore
setStore store s i = Map.insert s i store

-- | Abstract syntax for expressions
data Expr = CstI Int
          | Var  String
          | Prim String Expr Expr
          deriving Show

-- | Evaluate expression with environment to an integer
eval :: Expr -> NaiveStore -> Int
eval (CstI i) store = i
eval (Var x)  store = getStore store x
eval (Prim op e1 e2) store =
   let i1 = eval e1 store
       i2 = eval e2 store
   in  case op of
         "*"  -> i1 + i2
         "+"  -> i1 + i2
         "-"  -> i1 - i2
         "==" -> if i1 == i2 then 1 else 0
         "<"  -> if i1 <  i2 then 1 else 0
         _    -> error "unknown primitive"

-- | Abstract syntax for statements
data Stmt = Assign String Expr
          | If Expr Stmt Stmt
          | Block [Stmt]
          | For String Expr Expr Stmt
          | While Expr Stmt
          | Print Expr
          deriving Show

-- | Execument statement and update environment
exec :: Stmt -> NaiveStore -> IO NaiveStore
exec (Assign x e) store = return $
   setStore store x (eval e store)
exec (If e1 stmt1 stmt2) store =
   if   eval e1 store /= 0
   then exec stmt1 store
   else exec stmt2 store
exec (Block stmts) store =
   let loop ss sto =
         case ss of
            []         -> return sto
            (s1:srest) -> do sto' <-(exec s1 sto)
                             loop srest sto'
   in  loop stmts store
exec (For x estart estop stmt) store =
   let start = eval estart store
       stop  = eval estop  store
       loop i sto = if   i > stop
                    then return sto
                    else do sto' <- exec stmt (setStore store x i)
                            loop (i + 1) sto'
   in  loop start store
exec (While e stmt) store =
   let loop sto = if   (eval e sto) == 0
                  then return sto
                  else do sto'' <- exec stmt sto
                          loop sto''
   in  loop store
exec (Print e) store =
   do print (eval e store)
      return store

-- | Run program (statement)
run :: Stmt -> IO ()
run stmt = do
   exec stmt emptyStore
   return ()

-- | Example programs
ex1 :: Stmt
ex1 =
   Block [ Assign "sum" (CstI 0)
         , For "i" (CstI 0) (CstI 100) (Assign "sum" (Prim "+" (Var "sum") (Var "i")))
         , Print (Var "sum")
         ]

ex2 :: Stmt
ex2 =
   Block [ Assign "i"   (CstI 1)
         , Assign "sum" (CstI 0)
         , While (Prim "<" (Var "sum") (CstI 10000))
                 (Block [ Print (Var "sum")
                        , Assign "sum" (Prim "+" (Var "sum") (Var "i"))
                        , Assign "i"   (Prim "+" (CstI 1)    (Var "i"))])
         , Print (Var "i")
         , Print (Var "sum")
         ]

