{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Pervasives (
  Chr, Nat(Zero, Succ), Level(Hi, Lo), T, A, anything, type_of, Mono(..),
  ExistsPoly(..), Poly(..), polymap, Typeable, Void, trace, cast,
) where
import Data.Typeable (Typeable, cast)
import Data.Void (Void)
import Debug.Trace (trace)
import Base.Kinds (Chr {- , Sym -}, Nat(Zero, Succ), Level(Hi, Lo))

class (Typeable a, Typeable t) => A (t :: k -> *) (a :: k) where
  anything :: t a

type family T (t :: k -> *) :: k -> *

type_of :: A (T t) a => t a -> T t a
type_of a = anything

data Mono :: (k -> *) -> * where
  Mono :: A (T t) a => t a -> Mono t

data ExistsPoly :: (k -> *) -> k -> * where
  ExistsPoly :: forall t a. A (T t) a =>
    Poly t -> ExistsPoly t a

data Poly :: (k -> *) -> * where
  MonoP :: Mono t -> Poly t
  ForallP :: Int -> (forall a. A (T t) a => ExistsPoly t a) -> Poly t

polymap :: forall t u. T t ~ T u =>
  (forall a. A (T t) a => t a -> Poly u) -> Poly t -> Poly u
polymap f (MonoP (Mono a)) = f a
polymap f (ForallP num exists_poly) = ForallP num $ do_stuff exists_poly where
  do_stuff :: ExistsPoly t a -> ExistsPoly u a
  do_stuff (ExistsPoly poly) = ExistsPoly $ polymap f poly
