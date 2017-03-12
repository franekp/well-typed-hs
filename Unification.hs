{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Unification (unify, main_Unification) where
import Instances

synchronize_quantifiers :: Poly t -> Poly t -> (Poly t, Poly t)
synchronize_quantifiers (MonoP a) (MonoP b) = (MonoP a, MonoP b)
synchronize_quantifiers (MonoP a) b = (aa, bb) where
  (bb, aa) = synchronize_quantifiers b (MonoP a)
synchronize_quantifiers (ForallP id_a a_) b_ =
  let
    helper_left :: A (T t) a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_left (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = synchronize_quantifiers a_ b_ in ExistsPoly aa
    helper_right :: A (T t) a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
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

dump_typevars :: forall t. T t ~ Type => Poly t -> [Mono TypeVar]
dump_typevars (MonoP (Mono tt)) = do_stuff (type_of tt) [] where
  do_stuff :: A Type a => Type a -> [Mono TypeVar] -> [Mono TypeVar]
  do_stuff arg acc = case arg of
    a `ArrowTT` b -> do_stuff a $ do_stuff b $ acc
    TypeVarTT a -> Mono a:acc
    _ -> acc
dump_typevars (ForallP _ exists_poly) = result exists_poly where
  result :: ExistsPoly t TypeHole -> [Mono TypeVar]
  result (ExistsPoly poly) = dump_typevars poly

unpack_poly :: forall t. T t ~ Type => Poly t -> Poly t -> (Mono t, Mono t, VarMapping, Mono TypeVar)
unpack_poly a b =
  case start_var of
    Mono ZeroTV -> unpack_poly' ZeroTV a b $ VarMapping []
    Mono (SuccTV s) -> unpack_poly' (SuccTV s) a b $ VarMapping []
  where
    start_var = typevar_max_plus_one $ dump_typevars a ++ dump_typevars b
    unpack_poly' :: forall a. (A TypeVar a, A Type a) => TypeVar a -> Poly t -> Poly t
      -> VarMapping -> (Mono t, Mono t, VarMapping, Mono TypeVar)
    unpack_poly' last_tv (MonoP a) (MonoP b) m = (a, b, m, start_var)
    unpack_poly' last_tv
      (ForallP num_a (ExistsPoly poly_a :: ExistsPoly t a))
      (ForallP num_b (ExistsPoly poly_b :: ExistsPoly t a ))
      (VarMapping m) =
        if num_a /= num_b then error "synchronize_quantifiers not called before unpack_poly!" else
        unpack_poly' (SuccTV last_tv) poly_a poly_b $ VarMapping $ (num_a, Mono last_tv):m

data Constraint = Constraint (Mono TypeVar) (Mono Type)
  deriving Show

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

apply_constraint :: forall t. T t ~ Type => VarMapping -> Constraint -> Poly t -> Poly t
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
            do_stuff (ExistsPoly poly) = ExistsPoly $ helper poly $ TypeMapping $ (num, Mono (anything :: Type a)):m
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
      apply_type :: forall a. A Type a => Type a -> (forall b. A Type b => ExistsPoly t b) -> Poly t
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

-- TODO add a check to detect infinite types
gen_constraints :: (A Type a, A Type b) => Mono TypeVar -> Type a -> Type b -> [Constraint]
gen_constraints start_var (a `ArrowTT` b) (a' `ArrowTT` b') =
  gen_constraints start_var a a' ++ gen_constraints start_var b b'
gen_constraints start_var (TypeVarTT a) a' = if Mono a < start_var then [] else
  case a' of
    TypeVarTT a'' ->
      if Mono a == Mono a'' then [] else
      [Constraint (Mono a) (Mono a')]
    _ -> [Constraint (Mono a) (Mono a')]
gen_constraints start_var a (TypeVarTT a') =
  gen_constraints start_var (TypeVarTT a') a
gen_constraints _ _ _ = []

var_to_type :: Mono TypeVar -> Mono Type
var_to_type (Mono a) = Mono $ TypeVarTT a

apply_constraint_to_constraint_list ::
  Mono TypeVar -> Constraint -> [Constraint] -> [Constraint]
apply_constraint_to_constraint_list start_var (Constraint var replacement) li =
  concat $ map apply li where
    apply :: Constraint -> [Constraint]
    apply (Constraint v r) =
      let mapped_r = map_vars (\rv -> if rv /= var then var_to_type rv else replacement) r in
      if v /= var then
        [Constraint v $ mapped_r]
      else
        case (replacement, mapped_r) of
          (Mono a, Mono b) -> gen_constraints start_var a b

zip_quantifiers :: forall t u. T t ~ T u => Poly t -> Poly t
  -> (forall a b. (A (T t) a, A (T t) b) => t a -> t b -> Poly u) -> Poly u
zip_quantifiers (MonoP (Mono a)) (MonoP (Mono b)) cont = cont a b
zip_quantifiers (ForallP num_a exists_poly_a) (ForallP num_b exists_poly_b) cont =
  if num_a /= num_b then
    error "zip_quantifiers: quantifiers id mismatch"
  else
    ForallP num_a $ helper exists_poly_a exists_poly_b
  where
    helper :: A (T t) a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly u a
    helper (ExistsPoly poly_a) (ExistsPoly poly_b) =
      ExistsPoly $ zip_quantifiers poly_a poly_b cont
zip_quantifiers _ _ _ = error "zip_quantifiers: quantifier list length mismatch"

unify :: forall t u. (T t ~ Type, T u ~ Type) =>
  (forall a. A Type a => t a -> Mono Type) -> Poly t ->
  (forall a. A Type a => t a -> Mono Type) -> Poly t ->
  (forall a b. (A Type a, A Type b) => t a -> t b -> Poly u) -> Poly u
unify f_a a_input f_b b_input cont =
  let (a_poly, b_poly) = synchronize_quantifiers a_input b_input in
  let (a_mono, b_mono, m, start_var) = unpack_poly a_poly b_poly in
  let
    constraints = case (a_mono, b_mono) of
      (Mono a, Mono b) -> case (f_a a, f_b b) of
        (Mono a', Mono b') -> gen_constraints start_var a' b'
  in let
    helper :: [Constraint] -> Poly t -> Poly t -> (Poly t, Poly t)
    helper [] aa bb = (aa, bb)
    helper (c:cs) aa bb = res where
      res = helper
        (apply_constraint_to_constraint_list start_var c cs)
        (apply_constraint m c aa)
        (apply_constraint m c bb)
    (a_res, b_res) = helper constraints a_poly b_poly
  in zip_quantifiers a_res b_res cont

main_Unification = do
  let (e1, e2) = synchronize_quantifiers type_example_4 type_example_3
  print e1
  print e2
  print "----"
  let a = ZeroTV
  let b = SuccTV a
  let c = SuccTV b
  let d = SuccTV c
  let e = SuccTV d
  let f = SuccTV e
  let g = SuccTV f
  print $ unify
    (Mono . type_of) type_example_4
    (Mono . type_of) type_example_3
    (\a b -> MonoP $ Mono a)
  print $ unify
    (Mono . type_of) type_example_4
    (Mono . type_of) type_example_3
    (\a b -> MonoP $ Mono b)
