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

module Unification (unify) where

import Data.Typeable (Typeable, cast)

import Types
import Instances hiding (main)

make_quantifiers_common :: Poly t -> Poly t -> (Poly t, Poly t)
make_quantifiers_common (MonoP a) (MonoP b) = (MonoP a, MonoP b)
make_quantifiers_common (MonoP a) b = (aa, bb) where
  (bb, aa) = make_quantifiers_common b (MonoP a)
make_quantifiers_common (ForallP id_a a_) b_ =
  let
    helper_left :: Any a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_left (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = make_quantifiers_common a_ b_ in ExistsPoly aa
    helper_right :: Any a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_right (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = make_quantifiers_common a_ b_ in ExistsPoly bb
  in
  (ForallP id_a $ helper_left a_ (ExistsPoly b_), ForallP id_a $ helper_right a_ (ExistsPoly b_))

newtype VarMapping = VarMapping [(Int, Mono TypeVar)]
  deriving Show

newtype TypeMapping = TypeMapping [(Int, Mono Type)]
  deriving Show

mapping_int_to_var :: VarMapping -> Int -> Mono TypeVar
mapping_int_to_var (VarMapping []) a = error $ "Int \"" ++ show a ++ "\" not found in a VarMapping."
mapping_int_to_var (VarMapping ((num, var):t)) a =
  if a == num then var else mapping_int_to_var (VarMapping t) a

mapping_var_to_int :: VarMapping -> Mono TypeVar -> Int
mapping_var_to_int (VarMapping []) a = error $ "TypeVar \"" ++ show a ++ "\" not found in a VarMapping."
mapping_var_to_int (VarMapping ((num, var):t)) a =
  if a == var then num else mapping_var_to_int (VarMapping t) a

mapping_int_to_type :: TypeMapping -> Int -> Mono Type
mapping_int_to_type (TypeMapping []) a = error $ "Int \"" ++ show a ++ "\" not found in a TypeMapping."
mapping_int_to_type (TypeMapping ((num, tp):t)) a =
  if a == num then tp else mapping_int_to_type (TypeMapping t) a

mapping_type_to_int :: TypeMapping -> Mono Type -> Int
mapping_type_to_int (TypeMapping []) a = error $ "Type \"" ++ show a ++ "\" not found in a TypeMapping."
mapping_type_to_int (TypeMapping ((num, tp):t)) a =
  if a == tp then num else mapping_type_to_int (TypeMapping t) a

unpack_poly :: Poly t -> (Mono t, VarMapping)
unpack_poly arg = unpack_poly' ZeroTV arg $ VarMapping [] where
  unpack_poly' :: forall a t. Name a => TypeVar a -> Poly t -> VarMapping -> (Mono t, VarMapping)
  unpack_poly' last_tv (MonoP a) m = (a, m)
  unpack_poly' last_tv (ForallP num (ExistsPoly poly :: ExistsPoly t a)) (VarMapping m) =
    unpack_poly' (SuccTV last_tv) poly $ VarMapping $ (num, Mono last_tv):m

data Constraint = Constraint (Mono TypeVar) (Mono Type)

substitute_var :: Mono Type -> Mono TypeVar -> Mono Type -> Mono Type
((Mono t) `substitute_var` var) replacement = case t of
  a `ArrowTT` b -> let arrow (Mono a) (Mono b) = Mono $ a `ArrowTT` b in
    ((Mono a `substitute_var` var) replacement) `arrow` ((Mono b `substitute_var` var) replacement)
  IntTT -> Mono IntTT
  VoidTT -> Mono VoidTT
  TypeVarTT a ->
    if Mono a == var then replacement else Mono $ TypeVarTT a
  TypeHoleTT -> Mono TypeHoleTT

apply_constraint :: VarMapping -> Constraint -> Poly t -> Poly t
apply_constraint = undefined

gen_constraints :: Mono t -> Mono t -> [Constraint]
gen_constraints = undefined

unify :: forall t u. (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a b. t a -> t b -> Poly u) -> Poly u
unify f_a a_input f_b b_input cont =
  let (a_poly, b_poly) = make_quantifiers_common a_input b_input in
  let (a_mono, m) = unpack_poly a_poly in
  let (b_mono, _) = unpack_poly b_poly in

  undefined

main = do
  let (e1, e2) = make_quantifiers_common types_example_1 types_example_2
  putStrLn $ show $ e1
  putStrLn $ show $ e2
  putStrLn $ show $ unpack_poly e1
  putStrLn $ show $ unpack_poly e2
  let (mono1, m1) = unpack_poly e1
  putStrLn $ show $ m1 `mapping_int_to_var` 4
  putStrLn $ show $ m1 `mapping_var_to_int` (Mono ZeroTV)
  putStrLn $ show $ ((mono1 `substitute_var` Mono ZeroTV) (Mono VoidTT))
