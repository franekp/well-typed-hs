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

module Types where

import Data.Typeable (Typeable, Typeable1)
import Data.Void (Void)

data NameZero
  deriving Typeable
data NameSucc a
  deriving Typeable

data TypeHole
  deriving Typeable

data Type :: * -> * where
  ArrowTT :: Type a -> Type b -> Type (a -> b)
  IntTT :: Type Int
  VoidTT :: Type Void
  TypeVarTT :: Name a => TypeVar a -> Type a
  TypeHoleTT :: Type TypeHole

data TypeVar :: * -> * where
  ZeroTV :: TypeVar NameZero
  SuccTV :: TypeVar a -> TypeVar (NameSucc a)

class Typeable a => Any a where
  any_type :: Any a => Type a

class Any a => Name a where
  any_type_variable :: TypeVar a

data Mono :: (* -> *) -> * where
  Mono :: Typeable a => t a -> Mono t

data ExistsPoly :: (* -> *) -> * -> * where
  ExistsPoly :: Any a => Poly t -> ExistsPoly t a

data Poly :: (* -> *) -> * where
  MonoP :: Mono t -> Poly t
  ForallP :: Int -> (forall a. Any a => ExistsPoly t a) -> Poly t
