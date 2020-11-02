module Interp where

-- base
import           Prelude hiding (lookup)

-- containers
import qualified Data.Map as Map
import           Data.Map    (Map)

-- local
import           Absyn


-- | Simple environment operations

type Env a = [(String, a)]

lookup :: Show a => Env a -> String -> a
lookup [] x             = error $ show x ++ "  not found"
lookup ((y, v):yrest) x = if x == y then v else lookup yrest x

-- | A local variable environment also knows the next unused store location

type LocalEnv = (Env Int, Int)

-- | A function environment maps a function name to parameter list and body

type ParamDecs = [(Typ, String)]

type FunEnv = Env (ParamDecs, Stmt)

-- | A global environment consists of a global variable environment
--   and a global function environment

type GlobalEnv = (Env Int, FunEnv)

-- | The store maps addresses (ints) to values (ints):

type Addr = Int

type Store = Map Addr Int

type Value = Int

-- | Create empty store
emptyStore :: Store
emptyStore = Map.empty

-- | Get value of variable from store
getStore :: Store -> Addr -> Int
getStore store addr = case Map.lookup addr store of
                     Nothing -> error $   "getStore failed - "
                                       ++ show addr
                                       ++ " not found in store"
                     Just i  -> i

-- | Set value of variable in store
setStore :: Store -> Addr -> Value -> Store
setStore store addr value = Map.insert addr value store

initStore :: Addr -> Value -> Store -> Store
initStore loc n store =
    if n == 0
    then store
    else initStore (loc + 1) (n - 1) (setStore store loc (-999))

-- | Combined environment and store operations

-- | Extend local variable environment so it maps x to nextloc
--   (the next store location) and set store[nextloc] = v.

bindVar :: String -> Value -> LocalEnv -> Store -> (LocalEnv, Store)
bindVar x v (env, next_loc) store =
    let env1 = ((x, next_loc):env)
    in  ((env1, next_loc + 1), setStore store next_loc v)

bindVars :: [String] -> [Value] -> LocalEnv -> Store -> (LocalEnv, Store)
bindVars xs vs loc_env store =
    case (xs, vs) of
        ([], [])     -> (loc_env, store)
        (x:xr, v:vr) -> let (loc_env', store') = bindVar x v loc_env store
                        in  bindVars xr vr loc_env' store'
        _            -> error "parameter/argument mismatch"

-- | Allocate variable (int or pointer or array): extend environment so
--   that it maps variable to next available store location, and
--   initialize store location(s).

allocate :: (Typ, String) -> LocalEnv -> Store -> (LocalEnv, Store)
allocate (typ, x) (env, next_loc) store =
    let (next_loc', v, store') = case typ of
                                    TypA t (Just i) -> ( next_loc + i
                                                       , next_loc
                                                       , initStore next_loc i store )
                                    _               -> ( next_loc
                                                       , (-1)
                                                       , store )
    in  bindVar x v (env, next_loc') store'

-- | Build global environment of variables and functions.  For global
--   variables, store locations are reserved; for global functions, just
--   add to global function environment.

initEnvAndStore :: [TopDec] -> (LocalEnv, FunEnv, Store)
initEnvAndStore top_decs =
    let addv decs loc_env fun_env store =
            case decs of
                []                      -> (loc_env, fun_env, store)
                ((VarDec typ x) : decr) -> let (loc_env', store') = allocate (typ, x) loc_env store
                                           in  addv decr loc_env' fun_env store'
                ((FunDec _ f xs body) : decr) -> addv decr loc_env ((f, (xs, body)) : fun_env) store
    in  addv top_decs ([], 0) [] emptyStore

---------------------------------------------------------------------------

-- | Interpreting micro-C statements

exec :: Stmt -> LocalEnv -> GlobalEnv -> Store -> IO Store
exec (If e stmt1 stmt2) loc_env glo_env store =
    do (v, store') <- eval e loc_env glo_env store
       let stmt        = if v /= 0 then stmt1 else stmt2
       exec stmt loc_env glo_env store'
exec (While e body) loc_env glo_env store =
    let loop store' = do (v, store'') <- eval e loc_env glo_env store'
                         if v /= 0
                            then do store''' <- (exec body loc_env glo_env store'')
                                    loop store'''
                            else return store''
    in  loop store
exec (Expr e) loc_env glo_env store =
    fmap snd $ eval e loc_env glo_env store
exec (Block stmts) loc_env glo_env store =
    let loop ss (loc_env, store) =
            case ss of
                []     -> return store
                (s:sr) -> (stmtOrDec s loc_env glo_env store) >>= loop sr
    in  loop stmts (loc_env, store)
exec (Return _) _ _ _ =
    error "return not implemented"

stmtOrDec :: StmtOrDec -> LocalEnv -> GlobalEnv -> Store -> IO (LocalEnv, Store)
stmtOrDec stmtordec loc_env glo_env store =
    case stmtordec of
        Stmt stmt  -> do store' <- exec stmt loc_env glo_env store
                         return (loc_env, store')
        Dec typ x  -> return $ allocate (typ, x) loc_env store

-- | Evaluating micro-C expressions

eval :: Expr -> LocalEnv -> GlobalEnv -> Store -> IO (Value, Store)
eval (Access acc) loc_env glo_env store =
    do (loc, store') <- access acc loc_env glo_env store
       return (getStore store' loc, store')
eval (Assign acc e) loc_env glo_env store =
    do  (loc, store') <- access acc loc_env glo_env store
        (res,     store'') <- eval e loc_env glo_env store'
        return (res, setStore store'' loc res)
eval (CstI i) loc_env glo_env store =
    return (i, store)
eval (Addr acc) loc_env glo_env store =
    access acc loc_env glo_env store
eval (Prim1 op e) loc_env glo_env store =
    do  (i, store') <- eval e loc_env glo_env store
        case op of  "!"      -> return $ (if i == 0 then 1 else 0, store')
                    "printi" -> do print (show i)
                                   return (i, store')
                    "printc" -> do print (show i)
                                   return (i, store')
                    _        -> error $ "unknown primitive: " ++ op
eval (Prim2 op e1 e2) loc_env glo_env store =
    do  (i1, store1) <- eval e1 loc_env glo_env store
        (i2, store2) <- eval e2 loc_env glo_env store1
        let res = case op of "*"  -> i1 * i2
                             "+"  -> i1 + i2
                             "-"  -> i1 - i2
                             "/"  -> i1 `quot` i2
                             "%"  -> i1 `mod` i2
                             "==" -> if i1 == i2 then 1 else 0
                             "!=" -> if i1 /= i2 then 1 else 0
                             "<"  -> if i1 <  i2 then 1 else 0
                             "<=" -> if i1 <= i2 then 1 else 0
                             ">=" -> if i1 >= i2 then 1 else 0
                             ">"  -> if i1 >  i2 then 1 else 0
                             _    -> error $ "unknown primitive: " ++ op
        return (res, store2)
eval (AndAlso e1 e2) loc_env glo_env store =
    do res@(i1, store1) <- eval e1 loc_env glo_env store
       if i1 /= 0
           then eval e2 loc_env glo_env store1
           else return res
eval (OrElse e1 e2) loc_env glo_env store =
    do res@(i1, store1) <- eval e1 loc_env glo_env store
       if i1 /= 0
           then return res
           else eval e2 loc_env glo_env store
eval (Call f es) loc_env glo_env store =
    callFun f es loc_env glo_env store

access :: Access -> LocalEnv -> GlobalEnv -> Store -> IO (Value, Store)
access (AccVar x) loc_env glo_env store =
    return (lookup (fst loc_env) x, store)
access (AccDeref e) loc_env glo_env store =
    eval e loc_env glo_env store
access (AccIndex acc idx) loc_env glo_env store =
    do  (a, store')  <- access acc loc_env glo_env store
        let a_val    = getStore store' a
        (i, store'') <- eval idx loc_env glo_env store
        return (a_val + i, store'')

evals :: [Expr] -> LocalEnv -> GlobalEnv -> Store -> IO ([Value], Store)
evals es  loc_env glo_env store =
    case es of  []      -> return ([], store)
                (e:er)  -> do (v, store')   <- eval e loc_env glo_env store
                              (vr, store'') <- evals er loc_env glo_env store'
                              return ((v:vr), store'')

callFun :: String -> [Expr] -> LocalEnv -> GlobalEnv -> Store -> IO (Value, Store)
callFun f es loc_env glo_env store =
    do  let (_, next_loc)           = loc_env
            (var_env, fun_env)      = glo_env
            (param_decs, f_body)    = lookup fun_env f

        (vs, store')           <- evals es loc_env glo_env store

        let (f_body_env, store'')   = bindVars (map snd param_decs) vs (var_env, next_loc) store'

        store'''               <- exec f_body f_body_env glo_env store''
        return ((-111), store''')

-- | Interpret a complete micro-C program by initializing the store
--   and global environments, then invoking its `main' function.

run :: Program -> [Value] -> IO Store
run (Prog top_decs) vs =
    let ((var_env, next_loc), fun_env, store_0) = initEnvAndStore top_decs
        (main_params, main_body)                = lookup fun_env "main"
        (main_body_env, store_1)                = bindVars (map snd main_params) vs (var_env, next_loc) store_0
    in  exec main_body main_body_env (var_env, fun_env) store_1
