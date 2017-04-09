{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Ast where
import Base.Pervasives
import Base.Types

infixr `ConsST`
infixr `ConsEN`
infixr `RecordConsA`

data Store :: [*] -> * where
  NilST ::
    Store '[]
  ConsST :: A Type h =>
    h -> Store t -> Store (h ': t)

data Env :: [*] -> * where
  NilEN ::
    Env '[]
  ConsEN :: A Type h =>
    (String, Type h) -> Env t -> Env (h ': t)
  LetEN ::
    (String, Poly (Ast Hi e)) -> Env e -> Env e

newtype Builtin a = Builtin a

instance Show (Builtin a) where
  show _ = "<builtin>"

data Ast :: Level -> [*] -> * -> * where
  AddA ::
    Ast l e (Int -> Int -> Int)
  LiteralA ::
    Int -> Ast l e Int
  VarA :: A Type a =>
    Ast l (a ': e) a
  LiftA :: (A Type a, A Type b) =>
    Ast l e a -> Ast l (b ': e) a
  LambdaA :: (A Type a, A Type b) =>
    Ast l (a ': e) b -> Ast l e (a -> b)
  ErrorA :: A Type a =>
    String -> Ast l e a
  AppA :: (A Type a, A Type b) =>
    Ast l e (a -> b) -> Ast l e a -> Ast l e b
  RecordHeadA :: (A Type a, A FieldName f, A RecordType t) =>
    Ast l e (Record ('(f, a) ': t)) -> Ast l e a
  RecordTailA :: (A Type a, A FieldName f, A RecordType t) =>
    Ast l e (Record ('(f, a) ': t)) -> Ast l e (Record t)
  RecordNilA ::
    Ast l e (Record '[])
  RecordConsA :: (A Type a, A FieldName f, A RecordType t) =>
    (FieldName f, Ast l e a) -> Ast l e (Record t) -> Ast l e (Record ('(f, a) ': t))
  RecordGetA :: (A Type a, A FieldName f, A Type r) =>
    FieldName f -> Ast Hi e (HasField '(f, a) r) -> Ast Hi e a
  BuiltinA :: Builtin a -> Ast l e a

type instance T (Ast l e) = Type
