{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Types (module Base, module Types) where
import Base

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
