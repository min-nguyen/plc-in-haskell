{- Polymorphic type inference for a higher-order functional language    
   The operator (=) only requires that the arguments have the same type -}

{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module TypeInference where

import Prelude hiding (lookup)
import Data.List hiding (union, lookup)
import Control.Monad.State
import Control.Monad.ST 
import Data.STRef
import Data.Char
import Absyn


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
         | TypF (Typ s) (Typ s)                -- argumentType resultType
         | TypV (STRef s (TypeVar s))          -- type variable              
        deriving Eq 

data TyVarKind s 
               = NoLink String                 -- uninstantiated type var.   
               | LinkTo (Typ s)                   -- instantiated to typ        
              deriving Eq

type TypeVar s = (TyVarKind s, Int)              -- kind and binding level     

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
     

occurCheck :: TypeVar -> [TypeVar] -> ST s ()
occurCheck tyvar tyvars =
    if mem tyvar tyvars then error "type error: circularity" else return ()

pruneLevel :: Int -> [STRef s (TypeVar s)] -> ST s [()]
pruneLevel maxLevel tvs = 
    sequence $ map reduceLevel tvs 
    where reduceLevel :: STRef s (TypeVar s)  -> ST s () 
          reduceLevel tyvar = do (tvkind, level) <- readSTRef tyvar
                                 writeSTRef tyvar (tvkind, (min level maxLevel))

-- {- Make type variable tyvar equal to type t (by making tyvar link to t),
--    but first check that tyvar does not occur in t, and reduce the level
--    of all type variables in t to that of tyvar.  This is the `union'
--    operation in the union-find algorithm.  -}

linkTypeVarToType :: STRef s (TypeVar s) -> Typ s -> ST s ()
linkTypeVarToType tyvar t  =
    do (_, level) <- readSTRef tyvar
       fvs <- freeTypeVars t
       occurCheck tyvar fvs
       pruneLevel level fvs
       setTvKind (LinkTo t) tyvar

typeToString :: Typ s -> String
typeToString t =
    case t of
     TypI         -> "int"
     TypB         -> "bool"
     TypV _       -> error "typeToString impossible"
     TypF t1 t2   -> "function"
            
-- {- Unify two types, equating type variables with types as necessary -}

unify :: Typ s -> Typ s -> ST s ()
unify t1 t2 =
    do  t1' <- normType t1
        t2' <- normType t2
        case (t1', t2') of
            (TypI, TypI) -> return () 
            (TypB, TypB) -> return () 
            (TypF t11 t12, TypF t21 t22) -> do unify t11 t21 
                                               unify t12 t22
            (TypV tv1, TypV tv2) -> 
                do  (_, tv1level) <- readSTRef tv1
                    (_, tv2level) <- readSTRef tv2
                    if tv1 == tv2               then return ()
                    else if tv1level < tv2level then (linkTypeVarToType tv1 t2')
                                                else (linkTypeVarToType tv2 t1')
            (TypV tv1, _       ) -> linkTypeVarToType tv1 t2'
            (_,        TypV tv2) -> linkTypeVarToType tv2 t1'
            (TypI,     t) -> error ("type error: int and " ++ typeToString t)
            (TypB,     t) -> error ("type error: bool and " ++ typeToString t)
            (TypF _ _,   t) -> error ("type error: function and " ++ typeToString t)
 
-- {- Generate fresh type variables -}

type TyVarNo = Int

newTypeVar :: Int -> STRef s (TyVarNo) -> ST s (STRef s (TypeVar s))
newTypeVar level tyvarno = do
    let mkname i res =  if i < 26 then (intToDigit(97+i):res)
                        else mkname (i `quot` 26 -1) (intToDigit(97+(i `mod` 26)):res)
        intToName i = ('\'':(mkname i []))
    modifySTRef tyvarno (+1)
    num <- readSTRef tyvarno
    newSTRef (NoLink (intToName num), level)

-- {- Generalize over type variables not free in the context; that is,
--    over those whose level is higher than the current level: -}

generalize :: Int -> Typ s -> ST s (TypeScheme s)
generalize level t  = 
    let notfreeincontext tyvar = do (_, linkLevel) <- readSTRef tyvar 
                                    return (linkLevel > level)
    in  do ftvs <- (freeTypeVars t)
           tvs  <- filterM notfreeincontext ftvs
           return $ TypeScheme (unique tvs) t  

-- {- Copy a type, replacing bound type variables as dictated by tvenv,
--    and non-bound ones by a copy of the type linked to -}

copyType :: [(STRef s (TypeVar s), Typ s)] -> Typ s -> ST s (Typ s)
copyType subst t = 
    case t of
      TypV tyvar ->
        let loop subst1 =          
                case subst1 of 
                        ((tyvar1, type1):rest) 
                            -> if tyvar1 == tyvar then return type1 else loop rest
                        [] ->  do tyvar_deref <- readSTRef tyvar 
                                  case tyvar_deref of (NoLink _, _)  -> return t
                                                      (LinkTo t1, _) -> copyType subst t1 
        in loop subst
      TypF t1 t2  -> do t1' <- (copyType subst t1) 
                        t2' <- (copyType subst t2)
                        return $ TypF t1' t2'
      TypI        -> return TypI
      TypB        -> return TypB

-- -- {- Create a type from a type scheme (tvs, t) by instantiating all the
-- --    type scheme's parameters tvs with fresh type variables -}

specialize :: Int -> TypeScheme s -> STRef s TyVarNo -> ST s (Typ s)
specialize level (TypeScheme tvs t) tyvarno =
    let bindfresh tv = do tyvar <- newTypeVar level tyvarno
                          return (tv, TypV tyvar)
    in  case tvs of
         [] -> return t
         _  -> do subst <- mapM bindfresh tvs 
                  copyType subst t

-- -- {- Pretty-print type, using names 'a, 'b, ... for type variables -}
showType :: Typ s -> ST s String
showType t  =
    do t' <- normType t
       case t' of
                TypI         -> return "int"
                TypB         -> return "bool"
                TypV tyvar   -> do  tyvar_deref <- readSTRef tyvar
                                    case tyvar_deref of
                                                (NoLink name, _) -> return name
                                                _                -> error "showType impossible"
                TypF t1 t2   -> do t1' <- showType t1 
                                   t2' <- showType t2 
                                   return $ "(" ++ t1' ++ " -> " ++ t2' ++ ")"


-- -- {- Type inference helper function:
-- --    (typ lvl env e) returns the type of e in env at level lvl -}

typ :: Int ->  Env (TypeScheme s) -> Expr -> STRef s TyVarNo -> ST s (Typ s)
typ lvl env expr tyvarno = 
  do case expr of
      CstI i -> return TypI
      CstB b -> return TypB
      Var x  -> specialize lvl (lookup env x) tyvarno
      Prim ope e1 e2 -> 
        do  t1 <- typ lvl env e1 tyvarno
            t2 <- typ lvl env e2 tyvarno
            case ope of
                "*" -> (do unify TypI t1; unify TypI t2; return TypI)
                "+" -> (do unify TypI t1; unify TypI t2; return TypI)
                "-" -> (do unify TypI t1; unify TypI t2; return TypI)
                "=" -> (do unify t1 t2; return TypB)
                "<" -> (do unify TypI t1; unify TypI t2; return TypB)
                "&" -> (do unify TypB t1; unify TypB t2; return TypB)
                _   -> error ("unknown primitive " ++ ope) 
      Let x eRhs letBody -> 
        do  let lvl1     = lvl + 1
            resTy        <- typ lvl1 env eRhs tyvarno
            x_typescheme <- generalize lvl resTy
            let letEnv   = ((x, x_typescheme):env)
            typ lvl letEnv letBody tyvarno
      If e1 e2 e3 ->
        do  t2 <- typ lvl env e2 tyvarno
            t3 <- typ lvl env e3 tyvarno
            t1 <- typ lvl env e1 tyvarno
            unify TypB t1
            unify t2 t3
            return t2
      Letfun f x fBody letBody -> 
        do  let lvl1 = lvl + 1
            ftypevar <- newTypeVar lvl1 tyvarno
            xtypevar <- newTypeVar lvl1 tyvarno
            let fTyp = TypV ftypevar
                xTyp = TypV xtypevar
                fBodyEnv = ((x, TypeScheme [] xTyp) : (f, TypeScheme [] fTyp) : env)
            rTyp <- typ lvl1 fBodyEnv fBody tyvarno
            unify fTyp (TypF xTyp rTyp)
            f_typescheme <- generalize lvl fTyp
            let bodyEnv = ((f, f_typescheme) : env )
            typ lvl bodyEnv letBody tyvarno
      Call eFun eArg -> 
        do  tf <- typ lvl env eFun tyvarno
            tx <- typ lvl env eArg tyvarno
            rtypevar <- newTypeVar lvl tyvarno
            let tr = TypV rtypevar
            unify tf (TypF tx tr)
            return tr

-- {- Type inference: inferTyp' e0 prints the type of e0 assuming an empty environment -}

inferType :: Expr -> ST s (Typ s)
inferType e = do
    tyvarno <- newSTRef 0
    t <- (typ 0 [] e tyvarno)
    return t

inferType' :: Expr -> IO ()
inferType' e = do 
    t <- stToIO $ inferType e
    print (showType t)