{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Types where
import Base.Pervasives
import Base.Symbol

import Data.Dynamic (Dynamic)

typeof_polymap :: (T t ~ Type) => Poly t -> Poly Type
typeof_polymap = polymap (MonoP . Mono . type_of)

infixr `ConsRT`
infixr `ConsRC`
infixr :->

data HasField :: (Sym, *) -> * -> *

data Type :: * -> * where
  (:->) :: (A Type a, A Type b) =>
    Type a -> Type b -> Type (a -> b)
  IntT ::
    Type Int
  VoidT ::
    Type Void
  TypeVarT :: A TypeVar a =>
    TypeVar a -> Type (RuntimeTypeVar a)
  RecordT :: A RecordType a =>
    RecordType a -> Type (Record a)
  HasFieldT :: (A FieldName f, A Type a, A Type r) =>
    (FieldName f, Type a) -> Type r -> Type (HasField '(f, a) r)
  BoolT ::
    Type Bool
  MaybeT :: A Type a =>
    Type a -> Type (Maybe a)
  EitherT :: (A Type a, A Type b) =>
    Type a -> Type b -> Type (Either a b)
  CharT ::
    Type Char
  ListT :: A Type a =>
    Type a -> Type [a]
  IO_T :: A Type a =>
    Type a -> Type (IO a)
  DynamicT ::
    Type Dynamic

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
