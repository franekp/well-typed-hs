{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Ast where
import Base.Pervasives
import Base.Types

data Store :: * -> * where
  NilS ::
    Store Nil
  ConsS :: (A Type h, Typeable t) =>
    h -> Store t -> Store (Cons h t)

data Env :: * -> * where
  NilEN ::
    Env Nil
  ConsEN :: (A Type h, Typeable t) =>
    String -> Type h -> Env t -> Env (Cons h t)
  LetEN ::
    String -> Poly (Ast e) -> Env e -> Env e

data Ast :: * -> * -> * where
  AddA :: Typeable e =>
    Ast e (Int -> Int -> Int)
  LiteralA :: Typeable e =>
    Int -> Ast e Int
  VarA :: (A Type a, Typeable e) =>
    Ast (Cons a e) a
  LiftA :: (A Type a, A Type b, Typeable e) =>
    Ast e a -> Ast (Cons b e) a
  LambdaA :: (A Type a, A Type b, Typeable e) =>
    Ast (Cons a e) b -> Ast e (a -> b)
  ErrorA :: (A Type a, Typeable e) =>
    String -> Ast e a
  AppA :: (A Type a, A Type b, Typeable e) =>
    Ast e (a -> b) -> Ast e a -> Ast e b

type instance T (Ast e) = Type
