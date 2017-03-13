{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Types where
import Base.Pervasives
import Base.Symbol

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
    TypeVar a -> Type (RuntimeTypeVar a)
  RecordT :: A RecordType a =>
    RecordType a -> Type (Record a)

type instance T Type = Type

data RuntimeTypeVar :: Nat -> *

type instance T RuntimeTypeVar = TypeVar

data TypeVar :: Nat -> * where
  ZeroTV ::
    TypeVar Zero
  SuccTV :: A TypeVar a =>
    TypeVar a -> TypeVar (Succ a)

type instance T TypeVar = TypeVar

data FieldName :: Sym -> * where
  FieldName :: A Symbol f =>
    Symbol f -> FieldName f

type instance T FieldName = FieldName

data RecordType :: [(Sym, *)] -> * where
  NilRT ::
    RecordType '[]
  ConsRT :: (A FieldName f, A Type a, A RecordType r) =>
    (FieldName f, Type a) -> RecordType r -> RecordType ('(f, a) ': r)

type instance T RecordType = RecordType

data Record :: [(Sym, *)] -> * where
  NilRC ::
    Record '[]
  ConsRC :: (A FieldName f, A Type a, A RecordType r) =>
    (FieldName f, a) -> Record r -> Record ('(f, a) ': r)

type instance T Record = RecordType
