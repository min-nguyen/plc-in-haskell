{-
This module performs polymorphic type inference for a higher-order functional language.

The type inference algorithm used finds a "principal type scheme", which is the
most general type an expression can be given.

Type inference follows this general process:
    - Guess the type of an expression (normally this involves just inserting a placeholder)
    - Slowly equate types in until you achieve a "principal type scheme".

In this file we utilise the STRef monad, which gives us access to mutable references,
something that we require for this type inference algorithm.
You can find out more about this here: https://en.wikibooks.org/wiki/Haskell/Mutable_objects

NOTE:- The operator (=) only requires that the arguments have the same type.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module TypeInference where

-- base
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Char
import           Data.List           hiding (lookup, union)
import           Data.STRef
import           Prelude             hiding (lookup)

-- local
import           Absyn

-- -----------------------------------------------------------------------------
-- Environment
-- -----------------------------------------------------------------------------

-- | The env is represented by a set of pairs, connecting variable names to values.
type Env v = [(String, v)] -- (name of var, value of var)

-- | Will extract a variable from an environment when given the variables name.
--   Throws an error if the variable is not in the env.
lookup :: Env v -> String -> v
lookup [] x         = error (x ++ " not found")
lookup ((y, v):r) x = if x == y then v else lookup r x

-- -----------------------------------------------------------------------------
-- Ops on Sets of Type Variables
-- -----------------------------------------------------------------------------

-- | This section contains operations on sets of type variables that are represented as lists.
--
--    The functions are simple, but inefficient and compare type variables
--    by their string names, meaning that these operations are correct so long
--    as the variable names are distinct.
--
--    You may also notice that the functions are defined generally, so will work
--    on anything that has an 'Eq' instance.

-- | Checks to see if a given object is a member
mem :: Eq a => a -> [a] -> Bool
mem x []     = False
mem x (v:vr) = x == v || mem x vr
-- this could also be defined as a fold:
--mem x = foldr (\ v -> (||) (x == v)) False

-- | union (xs, ys) is the set of all elements in xs or ys, without duplicates.
union :: Eq a => ([a], [a]) -> [a]
union (xs, ys)
    = case xs of []     -> ys
                 (x:xr) -> if mem x ys
                           then union (xr, ys)
                           else x : union (xr, ys)

-- | unique xs is the set of members of xs, without duplicates.
unique :: Eq a => [a] -> [a]
unique = nub -- Defined in 'Data.List': https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:nub

-- -----------------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------------

-- | Data type representing the possible types of this language.
--   It says: a type can be an int, bool, function, or type variable:
data Typ s
    = TypI                       -- ^ Integer type.
    | TypB                       -- ^ Boolean type.
    | TypF (Typ s) (Typ s)       -- ^ Function type where 1st arg = argumentType and 2nd = resultType
    | TypV (STRef s (TypeVar s)) -- ^ Type variable.
    deriving Eq

-- Types that expand on what a type variable can be:

-- | Data type encapsulating what kinds of type variable there can be: instantiated,
--   an uninstantiated.
data TyVarKind s
    = NoLink String  -- ^ Uninstantiated type var.
    | LinkTo (Typ s) -- ^ Instantiated to typ.
    deriving Eq

-- | A type variable consists of its type (whether it is instantiated or not)
--   and the level at which it is bound
type TypeVar s = (TyVarKind s, Int) -- (kind, binding level)

-- -----------------------------------------------------------------------------
-- Type Scheme
-- -----------------------------------------------------------------------------

-- | A type scheme is a list of generalized type variables, and a type:
data TypeScheme s = TypeScheme
                        [STRef s (TypeVar s)] -- type variables
                        (Typ s)               -- type
    deriving Eq

-- -----------------------------------------------------------------------------
-- Type Inference
-- -----------------------------------------------------------------------------

-- | Type inference: 'inferType e0' returns the type of e0, if any.
inferType :: Expr -> ST s (Typ s)
inferType e = do
    tyvarno <- newSTRef 0 -- Instantiate a new thread.
    typ 0 [] e tyvarno    -- Perform type inference using 'typ'.

-- | Same as 'inferType',  but prints result using IO monad.
inferType' :: Expr -> IO ()
inferType' e = do
    t <- stToIO $ inferType e -- Convert to IO monad.
    print (showType t)        -- Print.

-- |Pretty-print type, using names 'a, 'b, ... for type variables.
showType :: Typ s -> ST s String
showType t  = do
    t' <- normType t
    case t' of
        TypI         -> return "int"
        TypB         -> return "bool"
        TypV tyvar   -> do
            tyvar_deref <- readSTRef tyvar
            case tyvar_deref of
                (NoLink name, _) -> return name
                _                -> error "showType impossible"
        TypF t1 t2   -> do
            t1' <- showType t1
            t2' <- showType t2
            return $ "(" ++ t1' ++ " -> " ++ t2' ++ ")"

-- -----------------------------------------------------------------------------
-- Type Inference - helper functions
-- -----------------------------------------------------------------------------

type TyVarNo = Int

-- | Type inference helper function:
typ
    :: Int                -- ^ Level to find type at (to find type of whole expression, set this to 0)
    -> Env (TypeScheme s) -- ^ Type variables found so far (initially [])
    -> Expr               -- ^ Expression to find type for.
    -> STRef s TyVarNo    -- ^ Reference to thread to operate in.
    -> ST s (Typ s)       -- ^ Type of expr at level lvl.
typ lvl env (CstI i) tyvarno = return TypI
typ lvl env (CstB b) tyvarno = return TypB
typ lvl env (Var x) tyvarno  = specialize lvl (lookup env x) tyvarno
typ lvl env (Prim ope e1 e2) tyvarno = do
    t1 <- typ lvl env e1 tyvarno
    t2 <- typ lvl env e2 tyvarno
    case ope of
        "*" -> do unify TypI t1; unify TypI t2; return TypI
        "+" -> do unify TypI t1; unify TypI t2; return TypI
        "-" -> do unify TypI t1; unify TypI t2; return TypI
        "=" -> do unify t1 t2; return TypB
        "<" -> do unify TypI t1; unify TypI t2; return TypB
        "&" -> do unify TypB t1; unify TypB t2; return TypB
        _   -> error ("unknown primitive " ++ ope)
typ lvl env (Let x eRhs letBody) tyvarno = do
    let lvl1 = lvl + 1
    resTy        <- typ lvl1 env eRhs tyvarno
    x_typescheme <- generalize lvl resTy
    let letEnv   = (x, x_typescheme) : env
    typ lvl letEnv letBody tyvarno
typ lvl env (If e1 e2 e3) tyvarno = do
    t2 <- typ lvl env e2 tyvarno
    t3 <- typ lvl env e3 tyvarno
    t1 <- typ lvl env e1 tyvarno
    unify TypB t1
    unify t2 t3
    return t2
typ lvl env (Letfun f x fBody letBody) tyvarno = do
    let lvl1 = lvl + 1
    ftypevar <- newTypeVar lvl1 tyvarno
    xtypevar <- newTypeVar lvl1 tyvarno
    let fTyp = TypV ftypevar
        xTyp = TypV xtypevar
        fBodyEnv = (x, TypeScheme [] xTyp) : (f, TypeScheme [] fTyp) : env
    rTyp <- typ lvl1 fBodyEnv fBody tyvarno
    unify fTyp (TypF xTyp rTyp)
    f_typescheme <- generalize lvl fTyp
    let bodyEnv = (f, f_typescheme) : env
    typ lvl bodyEnv letBody tyvarno
typ lvl env (Call eFun eArg) tyvarno = do
    tf <- typ lvl env eFun tyvarno
    tx <- typ lvl env eArg tyvarno
    rtypevar <- newTypeVar lvl tyvarno
    let tr = TypV rtypevar
    unify tf (TypF tx tr)
    return tr

-- Util:
-- -----------------------------------------------------------------------------

-- | Generate fresh type variables.
newTypeVar :: Int -> STRef s TyVarNo -> ST s (STRef s (TypeVar s))
newTypeVar level tyvarno = do
    let mkname i res =  if i < 26 then intToDigit (97+i) : res
                        else mkname (i `quot` 26 -1) (intToDigit(97+(i `mod` 26)):res)
        intToName i = '\'' : mkname i []
    modifySTRef tyvarno (+1)
    num <- readSTRef tyvarno
    newSTRef (NoLink (intToName num), level)

freeTypeVars :: Typ s -> ST s [STRef s (TypeVar s)]
freeTypeVars t = do
    t'' <- normType t
    case t'' of
        TypI        -> return []
        TypB        -> return []
        TypV tv     -> return [tv]
        TypF t1 t2  -> do
            t1' <- freeTypeVars t1
            t2' <- freeTypeVars t2
            return (union (t1', t2'))

-- | Normalize a type; make type variable point directly to the
--   associated type (if any).  This is the `find' operation, with path
--   compression, in the union-find algorithm.
normType :: Typ s -> ST s (Typ s)
normType t0@(TypV tyvar) = do
    (tyvarkind, lvl) <- readSTRef tyvar
    case (tyvarkind, lvl) of
        (LinkTo t1, _) -> do
            t2 <- normType t1
            setTvKind (LinkTo t2) tyvar
            return t2
        _ -> return t0
normType t0 = return t0

setTvKind :: TyVarKind s -> STRef s (TypeVar s) -> ST s ()
setTvKind newKind tyvar = do
    (kind, lvl) <- readSTRef tyvar
    writeSTRef tyvar (newKind, lvl)

-- Specialize:
-- -----------------------------------------------------------------------------

-- | Create a type from a type scheme (tvs, t) by instantiating all the
--   type scheme's parameters tvs with fresh type variables.
specialize :: Int -> TypeScheme s -> STRef s TyVarNo -> ST s (Typ s)
specialize level (TypeScheme tvs t) tyvarno =
    let bindfresh tv = do tyvar <- newTypeVar level tyvarno
                          return (tv, TypV tyvar)
    in  case tvs of
         [] -> return t
         _  -> do
                subst <- mapM bindfresh tvs
                copyType subst t

-- Copy a type, replacing bound type variables as dictated by tvenv,
-- and non-bound ones by a copy of the type linked to.
copyType :: [(STRef s (TypeVar s), Typ s)] -> Typ s -> ST s (Typ s)
copyType subst t@(TypV tyvar) =
    let loop subst1 =
            case subst1 of
                ((tyvar1, type1):rest) -> if tyvar1 == tyvar then return type1
                                                             else loop rest
                [] -> do
                    tyvar_deref <- readSTRef tyvar
                    case tyvar_deref of
                        (NoLink _, _)  -> return t
                        (LinkTo t1, _) -> copyType subst t1
    in loop subst
copyType subst (TypF t1 t2) = do
    t1' <- copyType subst t1
    t2' <- copyType subst t2
    return $ TypF t1' t2'
copyType _ TypI = return TypI
copyType _ TypB = return TypB

-- Unify:
-- -----------------------------------------------------------------------------

-- | Unify two types, equating type variables with types as necessary.
unify :: Typ s -> Typ s -> ST s ()
unify t1 t2 = do
    t1' <- normType t1
    t2' <- normType t2
    case (t1', t2') of
        (TypI, TypI) -> return ()
        (TypB, TypB) -> return ()
        (TypF t11 t12, TypF t21 t22) -> do unify t11 t21; unify t12 t22
        (TypV tv1, TypV tv2) ->
            do  (_, tv1level) <- readSTRef tv1
                (_, tv2level) <- readSTRef tv2
                if tv1 == tv2               then return ()
                else if tv1level < tv2level then linkTypeVarToType tv1 t2'
                                            else linkTypeVarToType tv2 t1'
        (TypV tv1, _       ) -> linkTypeVarToType tv1 t2'
        (_,        TypV tv2) -> linkTypeVarToType tv2 t1'
        (TypI,     t) -> error ("type error: int and " ++ typeToString t)
        (TypB,     t) -> error ("type error: bool and " ++ typeToString t)
        (TypF _ _,   t) -> error ("type error: function and " ++ typeToString t)

-- | Make type variable tyvar equal to type t (by making tyvar link to t),
--   but first check that tyvar does not occur in t, and reduce the level
--   of all type variables in t to that of tyvar.  This is the `union'
--   operation in the union-find algorithm.
linkTypeVarToType :: STRef s (TypeVar s) -> Typ s -> ST s ()
linkTypeVarToType tyvar t  =
    do (_, level) <- readSTRef tyvar
       fvs <- freeTypeVars t
       pruneLevel level fvs
       setTvKind (LinkTo t) tyvar

pruneLevel :: Int -> [STRef s (TypeVar s)] -> ST s [()]
pruneLevel maxLevel tvs =
    sequence $ map reduceLevel tvs
    where reduceLevel :: STRef s (TypeVar s)  -> ST s ()
          reduceLevel tyvar = do (tvkind, level) <- readSTRef tyvar
                                 writeSTRef tyvar (tvkind, min level maxLevel)

typeToString :: Typ s -> String
typeToString TypI         = "int"
typeToString TypB         = "bool"
typeToString (TypV _)     = error "typeToString impossible"
typeToString (TypF t1 t2) = "function"

-- Generalize:
-- -----------------------------------------------------------------------------

-- | Generalize over type variables not free in the context; that is,
--   over those whose level is higher than the current level:
generalize :: Int -> Typ s -> ST s (TypeScheme s)
generalize level t  =
    let notfreeincontext tyvar = do (_, linkLevel) <- readSTRef tyvar
                                    return (linkLevel > level)
    in  do ftvs <- freeTypeVars t
           tvs  <- filterM notfreeincontext ftvs
           return $ TypeScheme (unique tvs) t

-- TODO: tidy

-- QUESTION: why is this commented out?
-- | A type environment maps a program variable name to a typescheme.
-- type tenv = Env Typescheme

-- QUESTION: why is this commented out?
-- occurCheck :: TypeVar -> [TypeVar] -> ()
-- occurCheck tyvar tyvars =
--     if mem tyvar tyvars then error "type error: circularity" else ()

-- QUESTION:- why is this here - it is unused
setTvLvl :: Int -> STRef s (TypeVar s) -> ST s ()
setTvLvl newLvl tyvar = do
    (kind, lvl) <- readSTRef tyvar
    writeSTRef tyvar (kind, newLvl)
