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
    (String, Poly (Ast e)) -> Env e -> Env e

data Ast :: [*] -> * -> * where
  AddA :: Typeable e =>
    Ast e (Int -> Int -> Int)
  LiteralA :: Typeable e =>
    Int -> Ast e Int
  VarA :: (A Type a, Typeable e) =>
    Ast (a ': e) a
  LiftA :: (A Type a, A Type b, Typeable e) =>
    Ast e a -> Ast (b ': e) a
  LambdaA :: (A Type a, A Type b, Typeable e) =>
    Ast (a ': e) b -> Ast e (a -> b)
  ErrorA :: (A Type a, Typeable e) =>
    String -> Ast e a
  AppA :: (A Type a, A Type b, Typeable e) =>
    Ast e (a -> b) -> Ast e a -> Ast e b
  RecordHeadA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    Ast e (Record ('(f, a) ': t)) -> Ast e a
  RecordTailA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    Ast e (Record ('(f, a) ': t)) -> Ast e (Record t)
  RecordNilA :: Typeable e =>
    Ast e (Record '[])
  RecordConsA :: (A Type a, A FieldName f, A RecordType t, Typeable e) =>
    (FieldName f, Ast e a) -> Ast e (Record t) -> Ast e (Record ('(f, a) ': t))

type instance T (Ast e) = Type
