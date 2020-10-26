{-
This module performs polymorphic type inference for a higher-order functional language.

The type inference algorithm used finds a "principal type scheme", which is the
most general type an expression can be given.

Type inference follows this general process:
    - Guess the type of an expression (normally this involves just inserting a placeholder)
    - Slowly equate types in until you achieve a "principal type scheme".

NOTE:- The operator (=) only requires that the arguments have the same type.
-}

-- This is a Haskell language extension. This one, allows us to derive 'Semigroup'
-- and 'Monoid' class instances for 'TypeEnv'.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeInference where

-- base
import           Data.List (nub)

-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set

-- mtl
import           Control.Monad.Except
import           Control.Monad.State

-- local
import           Absyn

-- -----------------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------------
-- This type inference will aim to produce a 'TypeScheme' for an expression term,
-- and if it cannot it will raise a 'TypeError'.

-- Type scheme:
-- -----------------------------------------------------------------------------

-- Type scheme: these model polymorphic types, and indicate that the type variables bounds in
-- quantifier are polymorphic across the enclosed type and can be instantiated with any type
-- consistent with the signature.
-- This denotes a quantifier that for all type variables in [TVar], the type Type is returned.
data TypeScheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

-- Components of the Type Scheme:

newtype TVar = TV String   -- Type variable
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar               -- Type variable
  | TCon String             -- Constant type (e.g. Int or Bool)
  | TFun Type Type          -- Function type
  deriving (Show, Eq, Ord)

infixr `TFun`

typeInt :: Type
typeInt  = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

-- Type Environment:

-- Type environment: A mapping from a set of variables to their corresponding type scheme.
newtype TypeEnv = TypeEnv (Map.Map String TypeScheme)
  deriving (Semigroup, Monoid)

-- | The empty type environment.
emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

 -- | Adds a new entry to the type scheme
extend :: TypeEnv -> (String, TypeScheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

-- Type errors:
-- -----------------------------------------------------------------------------

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  deriving Show

-- -----------------------------------------------------------------------------
-- Inference
-- -----------------------------------------------------------------------------

-- Run type inference on a given Infer monad
runInfer :: Infer (Subst, Type) -> Either TypeError TypeScheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res
  where
    closeOver :: (Subst, Type) -> TypeScheme
    closeOver (sub, ty) = normalize sc
      where sc = generalize emptyTyenv (apply sub ty)

-- Generalize: Converting a τ type into a type ∀[α].τ where [α] is the set of fresh
-- type variables that have not yet appeared in the current type scheme. This is
-- done by closing over all free type variables in the type scheme.
generalize :: TypeEnv -> Type -> TypeScheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

-- Normalize a type; make type variable point directly to the
-- associated type (if any).  This is the `find' operation, with path
-- compression, in the union-find algorithm.
normalize :: TypeScheme -> TypeScheme
normalize (Forall ts body) = Forall (Map.elems ord) (normtype body)
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

-- Substitution:
-- -----------------------------------------------------------------------------

-- Substitution: A mapping from type variables to their corresponding type
type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

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

-- Infer Monad:
-- -----------------------------------------------------------------------------

-- The Infer monad: All our logic for type inference will live inside of this.
-- It is a monad transformer stack of ExceptT + State, allowing various error
-- reporting through 'TypeError' and statefully holding the fresh name supply
-- through (State Unique).
type Infer a = ExceptT TypeError (State Unique) a

-- Supply of fresh variables:

newtype Unique = Unique { count :: Int }

-- Initialise the fresh counter variable to 0
initUnique :: Unique
initUnique = Unique { count = 0 }

-- Fresh: Create a fresh type variable using the current counter value, and increment
-- the counter
fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- Instantiate: Converting a σ type into a τ type by creating fresh names for each type variable 
-- that does not appear in the current typing environment.
instantiate ::  TypeScheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

-- -----------------------------------------------------------------------------
-- Expr Inference and x and
-- -----------------------------------------------------------------------------

-- | Will attempt to match a given 'TypeEnv' and 'Expr' to a 'TypeScheme'
inferExpr :: TypeEnv -> Expr -> Either TypeError TypeScheme
inferExpr env = runInfer . inferExpr' env
  where
    -- Lifts Expr expressions into the Infer monad
    inferExpr' :: TypeEnv -> Expr -> Infer (Subst, Type)
    inferExpr' env (CstI _) = return (nullSubst, typeInt)
    inferExpr' env (CstB _) = return (nullSubst, typeBool)
    inferExpr' env (Var x) = lookupEnv env x
    inferExpr' env (Let x e1 e2)
      = do
          (s1, t1) <- inferExpr' env e1
          let env' = apply s1 env
              t'   = generalize env' t1
          (s2, t2) <- inferExpr' (env' `extend` (x, t')) e2
          return (s2 `compose` s1, t2)
    inferExpr' env (Prim op e1 e2)
      = do
          let expectedTy = ops op
          -- Derive actual type:
          (s1, t1) <- inferExpr' env e1
          (s2, t2) <- inferExpr' (apply s1 env) e2
          tv <- fresh                          -- var to be arg of function
          let primFunTy = TFun t1 (TFun t2 tv) -- derived type of function
          -- Check expected is same as derived:
          s3 <- unify (apply (s1 `compose` s2) primFunTy) expectedTy
          let overallSub = s1 `compose` s2 `compose` s3
          return (overallSub, apply s3 primFunTy)

    inferExpr' env (If cond tr fl)
      = do
          tv <- fresh
          let expectedTy = TFun typeBool (TFun tv (TFun tv tv))
          -- Derive actual type:
          (s1, t1) <- inferExpr' env cond
          (s2, t2) <- inferExpr' (apply s1 env) tr
          (s3, t3) <- inferExpr' (apply (s1 `compose` s2) env) fl
          let derived = TFun t1 (TFun t2 (TFun t3 tv))
          -- Check expected is same as derived:
          s4 <- unify (apply (s1 `compose` s2 `compose` s3) derived) expectedTy
          let overallSub = s1 `compose` s2 `compose` s3 `compose` s4
          return (overallSub, apply s4 derived)

    inferExpr' env (Letfun f x e1 e2)
      = do
        -- find type of function
        tv <- fresh
        let env' = env `extend` (x, Forall [] tv)  -- type for arg
        (s1, t1) <- inferExpr' env' e1             -- type for body
        let (s2, t2) = (s1, apply s1 (TFun tv t1)) -- overall body type
        -- continue as if it was a normal let
        let env'' = apply s2 env
            t'    = generalize env'' t2
        (s3, t3) <- inferExpr' (env'' `extend` (f, t')) e2
        return (s3 `compose` s1, t3)
    inferExpr' env (Call e1 e2)
      = do
        tv <- fresh
        (s1, t1) <- inferExpr' env e1
        (s2, t2) <- inferExpr' (apply s1 env) e2
        s3       <- unify (apply s2 t1) (TFun t2 tv)
        return (s3 `compose` s2 `compose` s1, apply s3 tv)

    -- Helper functions for 'inferExpr':

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

-- Examples:

exConst  = inferExpr emptyTyenv (CstI 7)
exBool   = inferExpr emptyTyenv (CstB True)
exVar    = inferExpr (TypeEnv $ Map.singleton "a" (Forall [TV "a"] (TCon "Bool"))) (Var "a")
exLet    = inferExpr emptyTyenv (Let "x" (CstI 7) (Var "x"))
exPrim   = inferExpr emptyTyenv (Prim "==" (CstI 7) (CstI 7))
exIf     = inferExpr emptyTyenv (If (CstB True) (CstI 7) (CstI 7))
exLetFun = inferExpr emptyTyenv (Letfun "f" "x" (Var "x") (Var "f"))
exCall   = inferExpr emptyTyenv (Call (Letfun "f" "x" (Var "x") (Var "f")) (CstI 7))
