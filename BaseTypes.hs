{-# LANGUAGE EmptyDataDecls,
 MultiParamTypeClasses,
 GADTs,
 RankNTypes,
 StandaloneDeriving,
 DeriveDataTypeable,
 ScopedTypeVariables,
 FunctionalDependencies,
 OverlappingInstances,
 FlexibleInstances,
 FlexibleContexts,
 ExistentialQuantification,
 UndecidableInstances,
 TypeFamilies #-}

module BaseTypes where

import Data.Typeable (Typeable, Typeable1)
import Data.Void (Void)

data NameZero
  deriving Typeable
data NameSucc a
  deriving Typeable

data TypeHole
  deriving Typeable

data Type :: * -> * where
  ArrowTT :: (Any a, Any b) => Type a -> Type b -> Type (a -> b)
  IntTT :: Type Int
  VoidTT :: Type Void
  TypeVarTT :: Name a => TypeVar a -> Type a
  TypeHoleTT :: Type TypeHole

data TypeVar :: * -> * where
  ZeroTV :: TypeVar NameZero
  SuccTV :: Name a => TypeVar a -> TypeVar (NameSucc a)

class Typeable a => Any a where
  get_type :: Any a => Type a

class Any a => Name a where
  type_variable_name :: TypeVar a -> String
  get_type_variable :: TypeVar a

data Mono t where
  Mono :: Any a => t a -> Mono t

data Poly t a where
  Poly :: Any a => Quantified t -> Poly t a

data Quantified t where
  MonoQ :: Mono t -> Quantified t
  PolyQ :: Int -> (forall a. Any a => Poly t a) -> Quantified t
