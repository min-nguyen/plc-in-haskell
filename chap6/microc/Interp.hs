module Interp where

import Prelude hiding (lookup)
import Absyn
import qualified Data.Map as Map
import Data.Map (Map)

type Env a = [(String, a)]

lookup :: Show a => Env a -> String -> a
lookup [] x             = error $ show x ++ "  not found"
lookup ((y, v):yrest) x = if x == y then v else lookup yrest x

type LocalEnv = (Env Int, Int)

type ParamDecs = [(Typ, String)]

type FunEnv = Env (ParamDecs, Stmt)

type GlobalEnv = (Env Int, FunEnv)

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

initEnvAndStore :: [TopDec] -> (LocalEnv, FunEnv, Store)
initEnvAndStore top_decs =
    let addv decs loc_env fun_env store =
            case decs of
                []                      -> (loc_env, fun_env, store)
                ((VarDec typ x) : decr) -> let (loc_env', store') = allocate (typ, x) loc_env store
                                           in  addv decr loc_env' fun_env store'
                ((FunDec _ f xs body) : decr) -> addv decr loc_env ((f, (xs, body)) : fun_env) store
    in  addv top_decs ([], 0) [] emptyStore

exec :: Stmt -> LocalEnv -> GlobalEnv -> Store -> Store
exec (If e stmt1 stmt2) loc_env glo_env store =
    let (v, store') = eval e loc_env glo_env store
        stmt        = if v /= 0 then stmt1 else stmt2
    in  exec stmt loc_env glo_env store'
exec (While e body) loc_env glo_env store =
    let loop store' = let (v, store'') = eval e loc_env glo_env store'
                      in  if   v /= 0
                          then loop (exec body loc_env glo_env store'')
                          else store''
    in  loop store
exec (Expr e) loc_env glo_env store =
    snd $ eval e loc_env glo_env store
exec (Block stmts) loc_env glo_env store =
    let loop ss (loc_env, store) =
            case ss of
                []     -> store
                (s:sr) -> loop sr (stmtOrDec s loc_env glo_env store)
    in  loop stmts (loc_env, store)
exec (Return _) _ _ _ =
    error "return not implemented"










