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

import BaseTypes
import Types hiding (main)

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

lift_quantifier :: Poly t -> Int -> Poly t
lift_quantifier = undefined
elim_quantifier :: Poly t -> Type a -> Poly t
elim_quantifier = undefined

data Mapping = Mapping [(Int, Mono TypeVar)]
  deriving Show

unpack_poly :: Poly t -> (Mono t, Mapping)
unpack_poly arg = unpack_poly' ZeroTV arg $ Mapping [] where
  unpack_poly' :: forall a t. Name a => TypeVar a -> Poly t -> Mapping -> (Mono t, Mapping)
  unpack_poly' last_tv (MonoP a) m = (a, m)
  unpack_poly' last_tv (ForallP num (ExistsPoly poly :: ExistsPoly t a)) (Mapping m) =
    unpack_poly' (SuccTV last_tv) poly $ Mapping $ (num, Mono last_tv):m

unify :: forall t u. (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a b. t a -> t b -> Poly u) -> Poly u
unify f_a a_ f_b b_ cont =
  let (a, b) = make_quantifiers_common a_ b_ in
  undefined

main = do
  let (e1, e2) = make_quantifiers_common types_example_1 types_example_2
  putStrLn $ show $ e1
  putStrLn $ show $ e2
  putStrLn $ show $ unpack_poly e1
  putStrLn $ show $ unpack_poly e2
