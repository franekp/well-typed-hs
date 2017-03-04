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

module Instances where

import Data.Void (Void)
import Data.Typeable (Typeable1, cast)
import qualified Data.Char

import Types

type_variable_name :: TypeVar a -> String
type_variable_name ZeroTV = "a"
type_variable_name (SuccTV a) = case type_variable_name a of
  'z':t -> 'a':'\'':t
  c:t -> (:t) $ Data.Char.chr $ (+1) $ Data.Char.ord c

instance Name NameZero where
  get_type_variable = ZeroTV

instance Name a => Name (NameSucc a) where
  get_type_variable = SuccTV get_type_variable

instance Any NameZero where
  get_type = TypeVarTT $ ZeroTV

instance Name a => Any (NameSucc a) where
  get_type = TypeVarTT $ SuccTV get_type_variable

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

deriving instance Eq (Type a)
deriving instance Typeable1 Type
instance Eq (Mono Type) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False

deriving instance Eq (TypeVar a)
deriving instance Typeable1 TypeVar
instance Eq (Mono TypeVar) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False

instance Show (Type a) where
  show ((a `ArrowTT` b) `ArrowTT` c) = "(" ++ show (a `ArrowTT` b) ++ ") -> " ++ show c
  show (a `ArrowTT` b) = show a ++ " -> " ++ show b
  show IntTT = "Int"
  show VoidTT = "Void"
  show (TypeVarTT a) = type_variable_name a
  show TypeHoleTT = "<type_hole>"

instance Show (Mono Type) where
  show (Mono a) = show a

instance Show (TypeVar a) where
  show ZeroTV = type_variable_name ZeroTV
  show (SuccTV a) = type_variable_name (SuccTV a)

instance Show (Mono TypeVar) where
  show (Mono a) = show a

instance Show (Mono t) => Show (Poly t) where
  show qq = "forall" ++ str ZeroTV qq where
    str :: (Name a, Show (Mono t)) => TypeVar a -> Poly t -> String
    str last_tv (MonoP tp) = ". " ++ show tp
    str last_tv (ForallP ident poly) = " " ++ show last_tv ++ do_stuff last_tv poly where
      do_stuff :: (Name a, Show (Mono t)) => TypeVar a -> ExistsPoly t a -> String
      do_stuff tv (ExistsPoly x) = str (SuccTV tv) x

types_example_1 = ForallP 4 (helper get_type)
  where
    helper :: forall a. Any a => (forall b. Any b => Type (a -> Int -> (a -> Int -> b) -> b)) -> ExistsPoly Type a
    helper arg =
      let
        inner :: Any a => (forall b. Any b => ExistsPoly Type b) -> ExistsPoly Type a
        inner polyast = ExistsPoly $ ForallP 5 polyast
      in inner ( (ExistsPoly . MonoP . Mono :: Any b => Type (a -> Int -> (a -> Int -> b) -> b) -> ExistsPoly Type b) arg)

types_example_2 = ForallP 3 $
    ((ExistsPoly . MonoP . Mono) :: Any a => Type ((a -> a) -> (a -> a) -> (a -> a)) -> ExistsPoly Type a)
    get_type

main = do
  putStrLn $ show $ types_example_1
  putStrLn $ show $ types_example_2
