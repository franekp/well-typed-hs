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

make_quantifiers_common :: Quantified t -> Quantified t -> (Quantified t, Quantified t)
make_quantifiers_common (MonoQ a) (MonoQ b) = (MonoQ a, MonoQ b)
make_quantifiers_common (MonoQ a) b = (aa, bb) where
  (bb, aa) = make_quantifiers_common b (MonoQ a)
make_quantifiers_common (PolyQ id_a a_) b_ =
  let
    helper_left :: Any a => Poly t a -> Poly t a -> Poly t a
    helper_left (Poly a_) (Poly b_) =
      let (aa, bb) = make_quantifiers_common a_ b_ in Poly aa
    helper_right :: Any a => Poly t a -> Poly t a -> Poly t a
    helper_right (Poly a_) (Poly b_) =
      let (aa, bb) = make_quantifiers_common a_ b_ in Poly bb
  in
  (PolyQ id_a $ helper_left a_ (Poly b_), PolyQ id_a $ helper_right a_ (Poly b_))

lift_quantifier :: Quantified t -> Int -> Quantified t
lift_quantifier = undefined
elim_quantifier :: Quantified t -> Type a -> Quantified t
elim_quantifier = undefined
unify :: forall t u. (forall a. Any a => t a -> Type a) -> Quantified t
  -> (forall a. Any a => t a -> Type a) -> Quantified t
  -> (forall a b. t a -> t b -> Quantified u) -> Quantified u
unify f_a a_ f_b b_ cont =
  let (a, b) = make_quantifiers_common a_ b_ in
  undefined

main = do
  let (e1, e2) = make_quantifiers_common types_example_1 types_example_2
  putStrLn $ show $ e1
  putStrLn $ show $ e2
