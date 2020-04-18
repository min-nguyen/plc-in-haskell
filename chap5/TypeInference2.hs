{-
This module performs polymorphic type inference for a higher-order functional language.

NOTE:- The operator (=) only requires that the arguments have the same type.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module TypeInference2 where

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

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid
import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set


newtype TVar = TV String   -- Type variable
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar               -- Type variable
  | TCon String             -- Constant type (e.g. Int or Bool)
  | TFun Type Type          -- Function type
  deriving (Show, Eq, Ord)

infixr `TFun`

-- Type scheme: these model polymorphic types, and indicate that the type variables bounds in
-- quantifier are polymorphic across the enclosed type and can be instantiated with any type
-- consistent with the signature.
-- This denotes a quantifier that for all type variables in [TVar], the type Type is returned.
data TypeScheme = Forall [TVar] Type  
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

-- Type environment: A mapping from a set of variables to their corresponding type scheme
newtype TypeEnv = TypeEnv (Map.Map String TypeScheme)
  deriving Monoid

data Unique = Unique { count :: Int }

-- Fresh: Create a fresh type variable using the current counter value, and increment
-- the counter
fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- The Infer monad: All our logic for type inference will live inside of this. 
-- It is a monad transformer stack of ExceptT + State, allowing various error 
-- reporting through TypeError and statefully holding the fresh name supply
-- through (State Unique).
type Infer a = ExceptT TypeError (State Unique) a

-- Substitution: A mapping from type variables to their corresponding type
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

-- Run type inference on a given Infer monad
runInfer :: Infer (Subst, Type) -> Either TypeError TypeScheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Subst, Type) -> TypeScheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

-- Initialise the fresh counter variable to 0
initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (String, TypeScheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> String -> Maybe TypeScheme
typeof (TypeEnv env) var_name = Map.lookup var_name env


-- Substitutable describes types where ..
class Substitutable a where
  -- We can apply a substitution to type variables in it
  apply :: Subst -> a -> a
  -- We can extract the set of free type variables from it
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (TFun t1 t2) = Set.union (ftv t1) (ftv t2)

instance Substitutable TypeScheme where
  apply s (Forall as t)   = Forall as (apply s' t)
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = Set.difference (ftv t) (Set.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

-- Unify: Attempt to unify two types
unify ::  Type -> Type -> Infer Subst
unify (TFun l  r) (TFun l' r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

-- Bind: Bind a type to a type variable
bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

-- OccursCheck: Check if an type variable 'a' occurs as a free type variable in 't'
occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- Instantiate: Converting a σ type into a τ type by creating fresh names for each type variable 
-- that does not appear in the current typing environment.
instantiate ::  TypeScheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

-- Generalize: Converting a τ type into a type ∀[α].τ where [α] is the set of fresh type variables that have not 
-- yet appeared in the current type scheme. This is done by closing over all free type variables in the type scheme.
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

-- Ops: Convert a binary operation into its corresponding type
ops :: String -> Type
ops "+"  = TFun typeInt (TFun typeInt typeInt)
ops "*"  = TFun typeInt (TFun typeInt typeInt)
ops "-"  = TFun typeInt (TFun typeInt typeInt)
ops "==" = TFun typeInt (TFun typeInt typeBool)

-- Lookup: Look up a variable x in the current type environment, returning an Infer monad
lookupEnv :: TypeEnv -> String -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of
    Var x -> lookupEnv env x
    Letfun f x e1 e2 -> do
        infer env (Let f (Lam x e1) e2)
    Lam x e -> do
        tv <- fresh
        let env' = env `extend` (x, Forall [] tv)
        (s1, t1) <- infer env' e
        return (s1, apply s1 (TFun tv t1))
    App e1 e2 -> do
        tv <- fresh
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (apply s1 env) e2
        s3       <- unify (apply s2 t1) (TFun t2 tv)
        return (s3 `compose` s2 `compose` s1, apply s3 tv)
    Let x e1 e2 -> do
        (s1, t1) <- infer env e1
        let env' = apply s1 env
            t'   = generalize env' t1
        (s2, t2) <- infer (env' `extend` (x, t')) e2
        return (s2 `compose` s1, t2)
    If cond tr fl -> do
        tv <- fresh
        inferPrim env [cond, tr, fl] (TFun typeBool (TFun tv (TFun tv tv)))
    Prim op e1 e2 -> do
        inferPrim env [e1, e2] (ops op)
    CstI _  -> return (nullSubst, typeInt)
    CstB _ -> return (nullSubst, typeBool)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . (TFun t))

inferExpr :: TypeEnv -> Expr -> Either TypeError TypeScheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: TypeScheme -> TypeScheme
normalize (Forall ts body) = Forall (fmap TV letters) (normtype body)
  where
    ord = Map.fromList $ zip (nub $ fv body) (fmap TV letters)

    fv (TVar (TV a))   = [a]
    fv (TFun a b) = fv a ++ fv b
    fv (TCon _)   = []
    
    normtype (TFun a b) = TFun (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar (TV a))   =
      case Map.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"