{-# LANGUAGE EmptyDataDecls,
 MultiParamTypeClasses,
 GADTs,
 RankNTypes,
 StandaloneDeriving,
 DeriveDataTypeable,
 ScopedTypeVariables,
 FunctionalDependencies,
 OverlappingInstances,
 FlexibleInstances,
 FlexibleContexts,
 ExistentialQuantification,
 UndecidableInstances,
 TypeFamilies #-}

module Types where

import Data.Void (Void)

import BaseTypes
import Names

instance Any a => Show (Type a) where
  show ((a `ArrowTT` b) `ArrowTT` c) = "(" ++ show (a `ArrowTT` b) ++ ") -> " ++ show c
  show (a `ArrowTT` b) = show a ++ " -> " ++ show b
  show IntTT = "Int"
  show VoidTT = "Void"
  show (TypeVarTT a) = type_variable_name a
  show TypeHoleTT = "<type_hole>"

instance Name a => Show (TypeVar a) where
  show a = type_variable_name a

instance (Any a, Any b) => Any (a -> b) where
  get_type = ArrowTT (get_type :: Type a) (get_type :: Type b)

instance Any Int where
  get_type = IntTT

instance Any Void where
  get_type = VoidTT

instance Any TypeHole where
  get_type = TypeHoleTT

type_of :: Any a => t a -> Type a
type_of a = get_type

instance Show (Quantified Type) where
  show qq = "forall" ++ str ZeroTV qq where
    str :: Name a => TypeVar a -> Quantified Type -> String
    str last_tv (NilQT (Opaque tp)) = ". " ++ show tp
    str last_tv (ConsQT ident poly) = " " ++ show last_tv ++ do_stuff last_tv poly where
      do_stuff :: Name a => TypeVar a -> Poly Type a -> String
      do_stuff tv (Poly x) = str (SuccTV tv) x

example1 = ConsQT 0 (helper get_type)
  where
    helper :: forall a. Any a => (forall b. Any b => Type (a -> Int -> (a -> Int -> b) -> b)) -> Poly Type a
    helper arg =
      let
        inner :: Any a => (forall b. Any b => Poly Type b) -> Poly Type a
        inner polyast = Poly $ ConsQT 1 polyast
      in inner ( (Poly . NilQT . Opaque :: Any b => Type (a -> Int -> (a -> Int -> b) -> b) -> Poly Type b) arg)

example2 = NilQT $ Opaque $ (get_type :: Type (Int -> Int -> Int))

main = putStrLn $ show $ example1
