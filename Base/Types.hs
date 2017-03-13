{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Types where
import Base.Pervasives
import Base.Symbols

infixr `ConsRT`
infixr `ConsRC`

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
  RecordT :: A RecordType a =>
    RecordType a -> Type (Record a)

type instance T Type = Type

data TypeVar :: * -> * where
  ZeroTV ::
    TypeVar Zero
  SuccTV :: A TypeVar a =>
    TypeVar a -> TypeVar (Succ a)

type instance T TypeVar = TypeVar

data FieldName :: [*] -> * where
  FieldName :: A Symbol f =>
    Symbol f -> FieldName f

type instance T FieldName = FieldName

data RecordType :: [([*], *)] -> * where
  NilRT ::
    RecordType '[]
  ConsRT :: (A FieldName f, A Type a, A RecordType r) =>
    (FieldName f, Type a) -> RecordType r -> RecordType ('(f, a) ': r)

type instance T RecordType = RecordType

data Record :: [([*], *)] -> * where
  NilRC ::
    Record '[]
  ConsRC :: (A FieldName f, A Type a, A RecordType r) =>
    (FieldName f, a) -> Record r -> Record ('(f, a) ': r)

type instance T Record = RecordType
