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

import Data.Typeable (Typeable, Typeable1, Typeable2)
import Data.Void (Void)

data Nil :: *
  deriving Typeable
data Cons :: * -> * -> *
  deriving Typeable
data Zero :: *
  deriving Typeable
data Succ :: * -> *
  deriving Typeable

data TypeHole
  deriving Typeable

data Type :: * -> * where
  ArrowTT :: (A Type a, A Type b) =>
    Type a -> Type b -> Type (a -> b)
  IntTT ::
    Type Int
  VoidTT ::
    Type Void
  TypeVarTT :: A TypeVar a =>
    TypeVar a -> Type a
  TypeHoleTT ::
    Type TypeHole

data TypeVar :: * -> * where
  ZeroTV ::
    TypeVar Zero
  SuccTV :: A TypeVar a =>
    TypeVar a -> TypeVar (Succ a)

class (Typeable a, Typeable1 t) => A t a where
  anything :: t a

type family T (t :: * -> *) :: * -> *

data Mono :: (* -> *) -> * where
  Mono :: A (T t) a => t a -> Mono t

data ExistsPoly :: (* -> *) -> * -> * where
  ExistsPoly :: A (T t) a => Poly t -> ExistsPoly t a

data Poly :: (* -> *) -> * where
  MonoP :: Mono t -> Poly t
  ForallP :: Int -> (forall a. A (T t) a => ExistsPoly t a) -> Poly t

polymap :: forall t u. T t ~ T u => (forall a. A (T t) a => t a -> Poly u) -> Poly t -> Poly u
polymap f (MonoP (Mono a)) = f a
polymap f (ForallP num exists_poly) = ForallP num $ do_stuff exists_poly where
  do_stuff :: forall a. ExistsPoly t a -> ExistsPoly u a
  do_stuff (ExistsPoly poly) = ExistsPoly $ polymap f poly
