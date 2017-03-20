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
  ConsST :: (A Type h, Typeable t) =>
    h -> Store t -> Store (h ': t)

data Env :: [*] -> * where
  NilEN ::
    Env '[]
  ConsEN :: (A Type h, Typeable t) =>
    (String, Type h) -> Env t -> Env (h ': t)
  LetEN ::
    (String, Poly (Ast Hi e)) -> Env e -> Env e

data Ast :: AstLevel -> [*] -> * -> * where
  AddA :: Typeable e =>
    Ast l e (Int -> Int -> Int)
  LiteralA :: Typeable e =>
    Int -> Ast l e Int
  VarA :: (A Type a, Typeable e) =>
    Ast l (a ': e) a
  LiftA :: (A Type a, A Type b, Typeable e) =>
    Ast l e a -> Ast l (b ': e) a
  LambdaA :: (A Type a, A Type b, Typeable e) =>
    Ast l (a ': e) b -> Ast l e (a -> b)
  ErrorA :: (A Type a, Typeable e) =>
    String -> Ast l e a
  AppA :: (A Type a, A Type b, Typeable e) =>
    Ast l e (a -> b) -> Ast l e a -> Ast l e b
  RecordHeadA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    Ast l e (Record ('(f, a) ': t)) -> Ast l e a
  RecordTailA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    Ast l e (Record ('(f, a) ': t)) -> Ast l e (Record t)
  RecordNilA :: Typeable e =>
    Ast l e (Record '[])
  RecordConsA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    (FieldName f, Ast l e a) -> Ast l e (Record t) -> Ast l e (Record ('(f, a) ': t))
  RecordGetA :: (A Type a, A FieldName f, A Type r, Typeable e) =>
    Ast l e (HasField '(f, a) r) -> FieldName f -> Ast l e a

type instance T (Ast l e) = Type
