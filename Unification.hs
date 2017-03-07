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

synchronize_quantifiers :: Poly t -> Poly t -> (Poly t, Poly t)
synchronize_quantifiers (MonoP a) (MonoP b) = (MonoP a, MonoP b)
synchronize_quantifiers (MonoP a) b = (aa, bb) where
  (bb, aa) = synchronize_quantifiers b (MonoP a)
synchronize_quantifiers (ForallP id_a a_) b_ =
  let
    helper_left :: Any a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_left (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = synchronize_quantifiers a_ b_ in ExistsPoly aa
    helper_right :: Any a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_right (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = synchronize_quantifiers a_ b_ in ExistsPoly bb
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

mapping_contains_var :: VarMapping -> Mono TypeVar -> Bool
mapping_contains_var (VarMapping []) a = False
mapping_contains_var (VarMapping ((num, var):t)) a =
  if a == var then True else mapping_contains_var (VarMapping t) a

mapping_int_to_type :: TypeMapping -> Int -> Mono Type
mapping_int_to_type (TypeMapping []) a = error $ "Int \"" ++ show a ++ "\" not found in a TypeMapping."
mapping_int_to_type (TypeMapping ((num, tp):t)) a =
  if a == num then tp else mapping_int_to_type (TypeMapping t) a

mapping_type_to_int :: TypeMapping -> Mono Type -> Int
mapping_type_to_int (TypeMapping []) a = error $ "Type \"" ++ show a ++ "\" not found in a TypeMapping."
mapping_type_to_int (TypeMapping ((num, tp):t)) a =
  if a == tp then num else mapping_type_to_int (TypeMapping t) a

typevar_max_plus_one :: [Mono TypeVar] -> Mono TypeVar
typevar_max_plus_one [] = Mono ZeroTV
typevar_max_plus_one li = helper li [] where
  helper :: [Mono TypeVar] -> [Mono TypeVar] -> Mono TypeVar
  helper (Mono h:t) res = case h of
    ZeroTV -> helper t res
    SuccTV a -> helper t (Mono a:res)
  helper [] res = case typevar_max_plus_one res of
    Mono (SuccTV a) -> Mono $ SuccTV $ SuccTV a
    Mono ZeroTV -> Mono $ SuccTV $ ZeroTV

dump_typevars :: Poly t -> [Mono TypeVar]
dump_typevars (MonoP (Mono tt)) = do_stuff (type_of tt) [] where
  do_stuff :: Any a => Type a -> [Mono TypeVar] -> [Mono TypeVar]
  do_stuff arg acc = case arg of
    a `ArrowTT` b -> do_stuff a $ do_stuff b $ acc
    TypeVarTT a -> Mono a:acc
    _ -> acc
dump_typevars (ForallP _ exists_poly) = result exists_poly where
  result :: ExistsPoly t TypeHole -> [Mono TypeVar]
  result (ExistsPoly poly) = dump_typevars poly

unpack_poly :: Poly t -> Poly t -> (Mono t, Mono t, VarMapping, Mono TypeVar)
unpack_poly a b =
  case start_var of
    Mono ZeroTV -> unpack_poly' ZeroTV a b $ VarMapping []
    Mono (SuccTV s) -> unpack_poly' (SuccTV s) a b $ VarMapping []
  where
    start_var = typevar_max_plus_one $ dump_typevars a ++ dump_typevars b
    unpack_poly' :: forall a t. Name a => TypeVar a -> Poly t -> Poly t
      -> VarMapping -> (Mono t, Mono t, VarMapping, Mono TypeVar)
    unpack_poly' last_tv (MonoP a) (MonoP b) m = (a, b, m, start_var)
    unpack_poly' last_tv
      (ForallP num_a (ExistsPoly poly_a :: ExistsPoly t a))
      (ForallP num_b (ExistsPoly poly_b :: ExistsPoly t a ))
      (VarMapping m) =
        if num_a /= num_b then error "synchronize_quantifiers not called before unpack_poly!" else
        unpack_poly' (SuccTV last_tv) poly_a poly_b $ VarMapping $ (num_a, Mono last_tv):m

data Constraint = Constraint (Mono TypeVar) (Mono Type)

map_vars :: (Mono TypeVar -> Mono Type) -> Mono Type -> Mono Type
map_vars f (Mono t) = case t of
  a `ArrowTT` b ->
    let
      a' = map_vars f (Mono a)
      b' = map_vars f (Mono b)
    in case (a', b') of
      (Mono a'', Mono b'') -> Mono $ a'' `ArrowTT` b''
  TypeVarTT tv -> f $ Mono tv
  x -> Mono x

apply_constraint :: forall t. Show (Mono t) => VarMapping -> Constraint -> Poly t -> Poly t
apply_constraint var_map (Constraint var_to_replace replacement) input = result where
  duplicate_foralls_except_one_and_step_inside_them ::
    Int -> (TypeMapping -> Poly t -> Poly t) -> Poly t
  duplicate_foralls_except_one_and_step_inside_them except_q cont = helper input (TypeMapping []) where
      helper :: Poly t -> TypeMapping -> Poly t
      helper current (TypeMapping m) = case current of
        MonoP mono -> cont (TypeMapping m) input
        ForallP num exists_poly ->
          let
            do_stuff :: forall a. ExistsPoly t a -> ExistsPoly t a
            do_stuff (ExistsPoly poly) = ExistsPoly $ helper poly $ TypeMapping $ (num, Mono (any_type :: Type a)):m
          in
          if num == except_q then
            case exists_poly of
              (ExistsPoly poly :: ExistsPoly t TypeHole) -> helper poly (TypeMapping m)
          else
            ForallP num $ do_stuff exists_poly
  result :: Poly t
  result = duplicate_foralls_except_one_and_step_inside_them
    (var_map `mapping_var_to_int` var_to_replace)
    do_something_inside
  do_something_inside :: TypeMapping -> Poly t -> Poly t
  do_something_inside type_map (MonoP mono) = MonoP mono
  do_something_inside type_map (ForallP num exists_poly) =
    let
      apply_type :: forall a. Any a => Type a -> (forall b. Any b => ExistsPoly t b) -> Poly t
      apply_type _ (ExistsPoly poly :: ExistsPoly t a) = poly
    in if num /= (var_map `mapping_var_to_int` var_to_replace) then
      case type_map `mapping_int_to_type` num of
        Mono tt -> do_something_inside type_map $ apply_type tt exists_poly
    else
      let
        true_replacement = make_true_replacement replacement
        make_true_replacement :: Mono Type -> Mono Type
        make_true_replacement tt = map_vars helper tt where
          helper :: Mono TypeVar -> Mono Type
          helper tv = if var_map `mapping_contains_var` tv
            then type_map `mapping_int_to_type` (var_map `mapping_var_to_int` tv)
            else case tv of
              Mono vv -> Mono $ TypeVarTT vv
      in case true_replacement of
        Mono tt -> do_something_inside type_map $ apply_type tt exists_poly

gen_constraints :: Mono t -> Mono t -> [Constraint]
-- FIXME: this should also take a parameter that indicates which variables
-- in the types are of interest and which should be ignored
gen_constraints = undefined

unify :: forall t u. (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a. Any a => t a -> Type a) -> Poly t
  -> (forall a b. t a -> t b -> Poly u) -> Poly u
unify f_a a_input f_b b_input cont =
  let (a_poly, b_poly) = synchronize_quantifiers a_input b_input in
  let (a_mono, b_mono, m, start_var) = unpack_poly a_poly b_poly in
  undefined

main = do
  let (e1, e2) = synchronize_quantifiers types_example_1 types_example_2
  putStrLn $ show $ e1
  putStrLn $ show $ e2
  putStrLn $ show $ unpack_poly e1 e2
  let (mono1, mono2, m, start_var) = unpack_poly e1 e2
  putStrLn $ show $ m `mapping_int_to_var` 4
  putStrLn $ show $ m `mapping_var_to_int` (Mono ZeroTV)
  let a = ZeroTV
  let b = SuccTV a
  let c = SuccTV b
  let d = SuccTV c
  let e = SuccTV d
  let f = SuccTV e
  let g = SuccTV f
  putStrLn $ show $ Mono f == Mono f
  putStrLn $ show $ apply_constraint (VarMapping [(4, Mono a), (5, Mono b), (3, Mono c)])
    (Constraint (Mono a) (Mono (ArrowTT (TypeVarTT c) (TypeVarTT c)))) e1
  putStrLn $ show $ apply_constraint (VarMapping [(4, Mono a), (5, Mono b), (3, Mono c)])
    (Constraint (Mono b) (Mono (ArrowTT IntTT (TypeVarTT a)))) e1
