{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Types where
import Pervasives

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

type instance T Type = Type

data TypeVar :: * -> * where
  ZeroTV ::
    TypeVar Zero
  SuccTV :: A TypeVar a =>
    TypeVar a -> TypeVar (Succ a)

type instance T TypeVar = TypeVar
