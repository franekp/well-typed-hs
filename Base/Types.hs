{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Types where
import Base.Pervasives

data Type :: * -> * where
  ArrowT :: (A Type a, A Type b) =>
    Type a -> Type b -> Type (a -> b)
  IntT ::
    Type Int
  VoidT ::
    Type Void
  TypeVarT :: A TypeVar a =>
    TypeVar a -> Type a
  HoleT ::
    Type Hole

type instance T Type = Type

data TypeVar :: * -> * where
  ZeroTV ::
    TypeVar Zero
  SuccTV :: A TypeVar a =>
    TypeVar a -> TypeVar (Succ a)

type instance T TypeVar = TypeVar
