{- Polymorphic type inference for a higher-order functional language    
   The operator (=) only requires that the arguments have the same type -}

{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module TypeInference where

import Prelude hiding (lookup)
import Data.List hiding (union, lookup)
import Control.Monad.State
import Control.Monad.ST 
import Data.STRef

--open Absyn

{- Environment operations -}

type Env v = [(String, v)]

lookup :: Env a -> String -> a 
lookup env x =
    case env of 
      []         -> error (x ++ " not found")
      ((y, v):r) -> if x == y then v else lookup r x

{- Operations on sets of type variables, represented as lists.  
   Inefficient but simple.  Basically compares type variables 
   on their string names.  Correct so long as all type variable names
   are distinct. -}

mem :: Eq a => a -> [a] -> Bool
mem x vs 
    = case vs of []     -> False
                 (v:vr) -> x == v || mem x vr

{- union (xs, ys) is the set of all elements in xs or ys, without duplicates -}

union :: Eq a => ([a], [a]) -> [a]
union (xs, ys) 
    = case xs of []     -> ys 
                 (x:xr) -> if mem x ys 
                           then union (xr, ys) 
                           else (x:(union (xr, ys)))

{- unique xs  is the set of members of xs, without duplicates -}

unique :: Eq a => [a] -> [a]
unique = nub

{- A type is int, bool, function, or type variable: -}

data Typ s 
         = TypI                                -- integers                   
         | TypB                                -- booleans                   
         | TypF (Typ s) (Typ s)                        -- argumentType resultType
         | TypV (STRef s (TypeVar s))    -- type variable              
        deriving Eq 

data TyVarKind s 
               = NoLink String                 -- uninstantiated type var.   
               | LinkTo (Typ s)                   -- instantiated to typ        
              deriving Eq

type TypeVar s = (TyVarKind s, Int) --ref                 (* kind and binding level     *)

{- A type scheme is a list of generalized type variables, and a type: -}

data TypeScheme s = TypeScheme [STRef s (TypeVar s)] (Typ s)     -- type variables and type    
    deriving Eq 

setTvKind :: TyVarKind s -> STRef s (TypeVar s) -> ST s ()
setTvKind newKind tyvar = do 
    (kind, lvl) <- readSTRef tyvar
    writeSTRef tyvar (newKind, lvl)


setTvLvl :: Int -> STRef s (TypeVar s) -> ST s ()
setTvLvl newLvl tyvar = do 
    (kind, lvl) <- readSTRef tyvar
    writeSTRef tyvar (kind, newLvl)

{- Normalize a type; make type variable point directly to the
   associated type (if any).  This is the `find' operation, with path
   compression, in the union-find algorithm. -}

normType :: Typ s -> ST s (Typ s)
normType t0 = 
    case t0 of
      TypV tyvar -> do (tyvarkind, lvl) <- readSTRef tyvar  
                       case (tyvarkind, lvl) of  
                            (LinkTo t1, _) -> do  t2 <- normType t1 
                                                  setTvKind (LinkTo t2) tyvar
                                                  return t2
                            _ ->  return t0 
      _ -> return t0

freeTypeVars :: Typ s -> ST s [STRef s (TypeVar s)]
freeTypeVars t = 
    do 
        t'' <- (normType t)
        case t'' of
                TypI        -> return []
                TypB        -> return []
                TypV tv     -> return [tv]
                TypF t1 t2  -> do t1' <- freeTypeVars t1
                                  t2' <- freeTypeVars t2 
                                  return (union (t1', t2'))
     

-- occurCheck :: TypeVar -> [TypeVar] -> ()
-- occurCheck tyvar tyvars =
--     if mem tyvar tyvars then error "type error: circularity" else ()

pruneLevel :: Int -> [STRef s (TypeVar s)] -> [STRef s (TypeVar s)]
pruneLevel maxLevel tvs = 
    map reduceLevel tvs 
    where reduceLevel :: STRef s (TypeVar s)  -> TypeVar 
          reduceLevel tyvar = do (tvkind, level) <- readSTRef tyvar
                                 writeSTRef tyvar (tvkind, (min level maxLevel))

-- pruneLevel :: Int -> Typ -> Typ
-- pruneLevel maxlevel t = 
--     case t of TypI       -> t 
--               TypB       -> t 
--               TypV tv    -> TypV (reduceLevel tv level)
--               TypF t1 t2 -> TypF (pruneLevel level t1) (pruneLevel level t2) 
-- {- Make type variable tyvar equal to type t (by making tyvar link to t),
--    but first check that tyvar does not occur in t, and reduce the level
--    of all type variables in t to that of tyvar.  This is the `union'
--    operation in the union-find algorithm.  -}

-- linkTypeVarToType :: TypeVar -> Type -> TypeVar
-- linkTypeVarToType tyvar t  =
--     let (_, level) = tyvar
--         t'  = pruneLevel level t;
--     in (LinkTo t', level)

-- typeToString :: Typ -> String
-- typeToString t =
--     case t of
--      TypI         -> "int"
--      TypB         -> "bool"
--      TypV _       -> error "typeToString impossible"
--      TypF t1 t2   -> "function"
            
-- {- Unify two types, equating type variables with types as necessary -}

-- unify :: Typ -> Typ -> (Typ, Typ)
-- unify t1 t2 =
--     let t1' = normType t1
--         t2' = normType t2
--     case (t1', t2') of
--       (TypI, TypI) -> (TypI, TypI)
--       (TypB, TypB) -> (TypB, TypB)
--       (TypF t11 t12, TypF t21 t22) -> let typ1 = unify t11 t21 
--                                           typ2 = unify t12 t22 
--                                       in  (typ1, typ2)
--       (TypV tv1, TypV tv2) -> 
--         let (_, tv1level) = tv1
--             (_, tv2level) = tv2
--         in  if tv1 == tv2               then (TypV tv1, TypV tv2)
--             else if tv1level < tv2level then (TypV (linkTypeVarToType tv1 t2'))
--                                         else (TypV (linkTypeVarToType tv2 t1'))
--       (TypV tv1, _       ) -> (TypV (linkTypeVarToType tv1 t2'), t2')
--       (_,        TypV tv2) -> (t1', TypV (linkTypeVarToType tv2 t1'))
--       (TypI,     t) -> error ("type error: int and " ++ typeToString t)
--       (TypB,     t) -> error ("type error: bool and " ++ typeToString t)
--       (TypF _,   t) -> error ("type error: function and " ++ typeToString t)

-- {- Generate fresh type variables -}

-- let tyvarno = ref 0

-- newTypVar :: Int -> State (Env TypeScheme) TypeVar
-- newTypVar level = 
--     let mkname i res = 
--             if i < 26 then char(97+i) :: res
--             else mkname (i/26-1) (char(97+i%26) :: res)
--         intToName i = new System.String(Array.ofList('\'' :: mkname i []))
--     tyvarno := !tyvarno + 1;
--     ref (NoLink (intToName (!tyvarno)), level)

-- {- Generalize over type variables not free in the context; that is,
--    over those whose level is higher than the current level: -}

-- generalize :: Int -> Typ -> TypeScheme
-- generalize level t  =
--     let notfreeincontext tyvar = 
--             let (_, linkLevel) = !tyvar 
--             in  linkLevel > level
--         tvs = filter notfreeincontext (freeTypeVars t)
--     in TypeScheme (unique tvs, t)  -- // The unique call seems unnecessary because freeTypeVars has no duplicates??

-- {- Copy a type, replacing bound type variables as dictated by tvenv,
--    and non-bound ones by a copy of the type linked to -}

-- copyType :: [(TypeVar, Typ)] -> Typ -> Typ 
-- copyType subst t = 
--     case t of
--       TypV tyvar ->
--         let loop subst1 =          
--             case subst1 of 
--                     ((tyvar1, type1):rest) 
--                         -> if tyvar1 = tyvar then type1 else loop rest
--                     [] -> match !tyvar with
--                             (NoLink _, _)  -> t
--                             (LinkTo t1, _) -> copyType subst t1
--         in loop subst
--       TypF(t1,t2) -> TypF (copyType subst t1) (copyType subst t2)
--       TypI        -> TypI
--       TypB        -> TypB

-- -- {- Create a type from a type scheme (tvs, t) by instantiating all the
-- --    type scheme's parameters tvs with fresh type variables -}

-- specialize :: Int -> TypeScheme -> Typ
-- specialize var (TypeScheme tvs t) =
--     let bindfresh tv = (tv, TypV (newTypeVar level))
--     in  case tvs of
--          [] -> t
--          _  -> let subst = map bindfresh tvs :: [(TypeVar, Typ)]
--                in  copyType subst t

-- -- {- Pretty-print type, using names 'a, 'b, ... for type variables -}
-- showType :: Typ -> String
-- showType t  =
--     let pr t = 
--         case normType of with
--           TypI         -> "int"
--           TypB         -> "bool"
--           TypV tyvar   -> 
--             case tyvar of
--                 (NoLink name, _) -> name
--                 _                -> error "showType impossible"
--           TypF t1 t2   -> "(" ++ pr t1 ++ " -> " ++ pr t2 ++ ")"
--     in pr t 

-- -- {- A type environment maps a program variable name to a typescheme -}

-- -- type tenv = Env Typescheme

-- -- {- Type inference helper function:
-- --    (typ lvl env e) returns the type of e in env at level lvl -}

-- typ :: Int -> Expr -> Env TypeScheme -> Typ
-- typ lvl expr env = do 
--     env <- get
--     case e of
--       CstI i -> TypI
--       CstB b -> TypB
--       Var x  -> specialize lvl (lookup env x)
--       Prim(ope, e1, e2) -> 
--         let t1 = typ lvl env e1
--             t2 = typ lvl env e2
--         in case ope of
--               "*" -> (unify TypI t1; unify TypI t2; TypI)
--               "+" -> (unify TypI t1; unify TypI t2; TypI)
--               "-" -> (unify TypI t1; unify TypI t2; TypI)
--               "=" -> (unify t1 t2; TypB)
--               "<" -> (unify TypI t1; unify TypI t2; TypB)
--               "&" -> (unify TypB t1; unify TypB t2; TypB)
--               _   -> failwith ("unknown primitive " + ope) 
--       Let(x, eRhs, letBody) -> 
--         let lvl1 = lvl + 1
--             resTy = typ lvl1 env eRhs
--             letEnv = (x, generalize lvl resTy) :: (Var, TypeScheme)
--         in  typ lvl letEnv letBody
--       If(e1, e2, e3) ->
--         let t2 = typ lvl env e2
--             t3 = typ lvl env e3
--             unify TypB (typ lvl env e1);
--             unify t2 t3;
--         in  t2
--     | Letfun(f, x, fBody, letBody) -> 
--       let lvl1 = lvl + 1
--       let fTyp = TypV(newTypeVar lvl1)
--       let xTyp = TypV(newTypeVar lvl1)
--       let fBodyEnv = (x, TypeScheme([], xTyp)) 
--                       :: (f, TypeScheme([], fTyp)) :: env
--       let rTyp = typ lvl1 fBodyEnv fBody
--       let _    = unify fTyp (TypF(xTyp, rTyp))
--       let bodyEnv = (f, generalize lvl fTyp) :: env 
--       typ lvl bodyEnv letBody
--     | Call(eFun, eArg) -> 
--       let tf = typ lvl env eFun 
--       let tx = typ lvl env eArg
--       let tr = TypV(newTypeVar lvl)
--       unify tf (TypF(tx, tr));
--       tr

-- {- Type inference: tyinf e0 returns the type of e0, if any -}

-- let rec tyinf e0 = typ 0 [] e0

-- let inferType e = 
--     (tyvarno := 0;
--      showType (tyinf e))
