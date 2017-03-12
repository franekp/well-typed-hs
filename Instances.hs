{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Instances (module Types, module Instances) where
import Types
import qualified Data.Char

type instance T Type = Type
type instance T TypeVar = TypeVar

instance A TypeVar Zero where
  anything = ZeroTV

instance A TypeVar a => A TypeVar (Succ a) where
  anything = SuccTV anything

instance A TypeVar a => A Type a where
  anything = TypeVarTT anything

instance (A Type a, A Type b) => A Type (a -> b) where
  anything = ArrowTT (anything :: Type a) (anything :: Type b)

instance A Type Int where
  anything = IntTT

instance A Type Void where
  anything = VoidTT

instance A Type TypeHole where
  anything = TypeHoleTT

type_of :: A (T t) a => t a -> T t a
type_of a = anything

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

instance Ord (Mono TypeVar) where
  Mono ZeroTV <= Mono _ = True
  Mono (SuccTV a) <= Mono ZeroTV = False
  Mono (SuccTV a) <= Mono (SuccTV b) = Mono a <= Mono b

instance Show (Type a) where
  show ((a `ArrowTT` b) `ArrowTT` c) = "(" ++ show (a `ArrowTT` b) ++ ") -> " ++ show c
  show (a `ArrowTT` b) = show a ++ " -> " ++ show b
  show IntTT = "Int"
  show VoidTT = "Void"
  show (TypeVarTT a) = show a
  show TypeHoleTT = "<type_hole>"

instance Show (Mono Type) where
  show (Mono a) = show a

instance Show (TypeVar a) where
  show ZeroTV = "a"
  show (SuccTV a) = case show a of
    'z':t -> 'a':'\'':t
    c:t -> (:t) $ Data.Char.chr $ (+1) $ Data.Char.ord c

instance Show (Mono TypeVar) where
  show (Mono a) = show a

instance (Show (Mono t), T t ~ Type) => Show (Poly t) where
  show qq = "forall" ++ str ZeroTV qq where
    str :: (A TypeVar a, A Type a, Show (Mono t)) => TypeVar a -> Poly t -> String
    str last_tv (MonoP tp) = ". " ++ show tp
    str last_tv (ForallP ident poly) = " " ++ show last_tv ++ show ident ++ do_stuff last_tv poly where
      do_stuff :: (A TypeVar a, A Type a, Show (Mono t)) => TypeVar a -> ExistsPoly t a -> String
      do_stuff tv (ExistsPoly x) = str (SuccTV tv) x

type_example_1 =
  ForallP 1 (ExistsPoly $
  ForallP 2 (ExistsPoly $
    MonoP $ Mono $ (anything :: Type (a -> (a -> b) -> b))
  :: forall b. A Type b => ExistsPoly Type b)
  :: forall a. A Type a => ExistsPoly Type a)

type_example_2 =
  ForallP 3 (ExistsPoly $
    MonoP $ Mono $ (anything :: Type ((a -> a) -> (a -> a) -> a -> a))
  :: forall a. A Type a => ExistsPoly Type a)

type_example_3 =
  ForallP 4 (ExistsPoly $
  ForallP 5 (ExistsPoly $
  ForallP 6 (ExistsPoly $
    MonoP $ Mono $ (anything :: Type ((a -> b) -> (b -> c) -> a -> c))
  :: forall c. A Type c => ExistsPoly Type c)
  :: forall b. A Type b => ExistsPoly Type b)
  :: forall a. A Type a => ExistsPoly Type a)

type_example_4 =
  ForallP 7 (ExistsPoly $
  ForallP 8 (ExistsPoly $
    MonoP $ Mono $ (anything :: Type ((a -> b) -> (b -> a) -> a -> a))
  :: forall b. A Type b => ExistsPoly Type b)
  :: forall a. A Type a => ExistsPoly Type a)

main_Instances = do
  putStrLn $ show $ type_example_1
  putStrLn $ show $ type_example_2
  putStrLn $ show $ type_example_3
  putStrLn $ show $ type_example_4
