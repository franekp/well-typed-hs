{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Unify (unify{-, test_unify-}) where
import Base

synchronize_quantifiers :: Poly t -> Poly t -> (Poly t, Poly t)
synchronize_quantifiers = synchronize_quantifiers' 0
synchronize_quantifiers' :: Int -> Poly t -> Poly t -> (Poly t, Poly t)
synchronize_quantifiers' i (MonoP a) (MonoP b) = (MonoP a, MonoP b)
synchronize_quantifiers' i (MonoP a) b = (aa, bb) where
  (bb, aa) = synchronize_quantifiers' i b (MonoP a)
synchronize_quantifiers' i (ForallP id_a a_) b_ =
  let
    -- TODO: optimize this!
    helper_left :: A (T t) a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_left (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = synchronize_quantifiers' (i+1) a_ b_ in ExistsPoly aa
    helper_right :: A (T t) a => ExistsPoly t a -> ExistsPoly t a -> ExistsPoly t a
    helper_right (ExistsPoly a_) (ExistsPoly b_) =
      let (aa, bb) = synchronize_quantifiers' (i+1) a_ b_ in ExistsPoly bb
  in
  (ForallP i $ helper_left a_ (ExistsPoly b_), ForallP i $ helper_right a_ (ExistsPoly b_))

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
    a :-> b -> do_stuff a $ do_stuff b $ acc
    IntT -> acc
    VoidT -> acc
    TypeVarT a -> Mono a:acc
    HasFieldT (f, a) rest -> do_stuff a $ do_stuff rest $ acc
    RecordT NilRT -> acc
    RecordT (ConsRT (f, a) rest) -> do_stuff a $ do_stuff (RecordT rest) $ acc
    BoolT -> acc
    MaybeT t -> do_stuff t $ acc
    EitherT l r -> do_stuff l $ do_stuff r $ acc
    CharT -> acc
    ListT t -> do_stuff t $ acc
    IO_T t -> do_stuff t $ acc
    DynamicT -> acc
    UnitT -> acc
    PairT a b -> do_stuff a $ do_stuff b $ acc
    TripleT a b c -> do_stuff a $ do_stuff b $ do_stuff c $ acc
dump_typevars (ForallP _ exists_poly) = result exists_poly where
  result :: ExistsPoly t Void -> [Mono TypeVar]
  result (ExistsPoly poly) = dump_typevars poly

unpack_poly :: forall t. T t ~ Type => Poly t -> Poly t -> (Mono t, Mono t, VarMapping, Mono TypeVar)
unpack_poly a b =
  case start_var of
    Mono ZeroTV -> unpack_poly' ZeroTV a b $ VarMapping []
    Mono (SuccTV s) -> unpack_poly' (SuccTV s) a b $ VarMapping []
  where
    start_var = typevar_max_plus_one $ dump_typevars a ++ dump_typevars b
    unpack_poly' :: forall a. A TypeVar a => TypeVar a -> Poly t -> Poly t
      -> VarMapping -> (Mono t, Mono t, VarMapping, Mono TypeVar)
    unpack_poly' last_tv (MonoP a) (MonoP b) m = (a, b, m, start_var)
    unpack_poly' last_tv
      (ForallP num_a (ExistsPoly poly_a :: ExistsPoly t (RuntimeTypeVar a)))
      (ForallP num_b (ExistsPoly poly_b :: ExistsPoly t (RuntimeTypeVar a)))
      (VarMapping m) =
        if num_a /= num_b then error "synchronize_quantifiers not called before unpack_poly!" else
        unpack_poly' (SuccTV last_tv) poly_a poly_b $ VarMapping $ (num_a, Mono last_tv):m
    unpack_poly' _ _ _ _ = error "unreachable"

data Constraint = Constraint (Mono TypeVar) (Mono Type)
  deriving Show

map_vars :: (Mono TypeVar -> Mono Type) -> Mono Type -> Mono Type
map_vars f (Mono t) = case t of
  a :-> b ->
    let
      a' = map_vars f (Mono a)
      b' = map_vars f (Mono b)
    in case (a', b') of
      (Mono a'', Mono b'') -> Mono $ a'' :-> b''
  TypeVarT tv -> f $ Mono tv
  IntT -> Mono t
  VoidT -> Mono t
  HasFieldT (ff, a) rest ->
    let
      a' = map_vars f (Mono a)
      rest' = map_vars f (Mono rest)
    in case (a', rest') of
      (Mono a'', Mono rest'') -> Mono $ HasFieldT (ff, a'') rest''
  RecordT NilRT -> Mono t
  RecordT (ConsRT (ff, a) rest) ->
    let
      a' = map_vars f (Mono a)
      rest' = map_vars f (Mono (RecordT rest))
    in case (a', rest') of
      (Mono a'', Mono (RecordT rest'')) -> Mono $ RecordT $ ConsRT (ff, a'') rest''
      (_, _) -> error "unreachable code"
  BoolT -> Mono t
  MaybeT t ->
    let
      t' = map_vars f (Mono t)
    in case t' of
      Mono t'' -> Mono $ MaybeT t''
  EitherT l r ->
    let
      l' = map_vars f (Mono l)
      r' = map_vars f (Mono r)
    in case (l', r') of
      (Mono l'', Mono r'') -> Mono $ EitherT l'' r''
  CharT -> Mono t
  ListT t ->
    let
      t' = map_vars f (Mono t)
    in case t' of
      Mono t'' -> Mono $ ListT t''
  IO_T t ->
    let
      t' = map_vars f (Mono t)
    in case t' of
      Mono t'' -> Mono $ IO_T t''
  DynamicT -> Mono t
  UnitT -> Mono t
  PairT a b ->
    let
      a' = map_vars f (Mono a)
      b' = map_vars f (Mono b)
    in case (a', b') of
      (Mono a'', Mono b'') -> Mono $ PairT a'' b''
  TripleT a b c ->
    let
      a' = map_vars f (Mono a)
      b' = map_vars f (Mono b)
      c' = map_vars f (Mono c)
    in case (a', b', c') of
      (Mono a'', Mono b'', Mono c'') -> Mono $ TripleT a'' b'' c''

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
              (ExistsPoly poly :: ExistsPoly t Void) -> helper poly (TypeMapping m)
          else
            ForallP num $ do_stuff exists_poly
  result :: Poly t
  result = -- trace (show var_to_replace ++ "~~~>" ++ show replacement ++ "    (" ++ show var_map ++ ")" ) $
    duplicate_foralls_except_one_and_step_inside_them
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
              Mono vv -> Mono $ TypeVarT vv
      in case true_replacement of
        Mono tt -> do_something_inside type_map $ apply_type tt exists_poly

gen_constraints_assert_has_field :: (A Type a, A Type b, A FieldName f) =>
  Mono TypeVar -> FieldName f -> Type a -> Type b -> [Constraint]
gen_constraints_assert_has_field start_var f a ((f', a') `HasFieldT` b) =
  (if Mono f == Mono f' then gen_constraints start_var a a' else [])
  ++ gen_constraints_assert_has_field start_var f a b
gen_constraints_assert_has_field start_var f a (RecordT ((f', a') `ConsRT` b)) =
  (if Mono f == Mono f' then gen_constraints start_var a a' else [])
  ++ gen_constraints_assert_has_field start_var f a (RecordT b)
gen_constraints_assert_has_field start_var _ _ _ = []

-- TODO add a check to detect infinite types
gen_constraints :: (A Type a, A Type b) => Mono TypeVar -> Type a -> Type b -> [Constraint]

gen_constraints start_var (TypeVarT a) (TypeVarT b) =
  if Mono a < start_var && Mono b < start_var then [] else
  if Mono a == Mono b then [] else
  if Mono a < Mono b then [Constraint (Mono b) (Mono (TypeVarT a))] else
  [Constraint (Mono a) (Mono (TypeVarT b))]
gen_constraints start_var (TypeVarT a) a' = if Mono a < start_var then [] else
  case a' of
    TypeVarT a'' ->
      if Mono a == Mono a'' then [] else
      [Constraint (Mono a) (Mono a')]
    _ -> [Constraint (Mono a) (Mono a')]
gen_constraints start_var a (TypeVarT a') =
  gen_constraints start_var (TypeVarT a') a

gen_constraints start_var ((f, t) `HasFieldT` rest) a =
  gen_constraints_assert_has_field start_var f t a ++ gen_constraints start_var rest a
gen_constraints start_var a ((f, t) `HasFieldT` rest) =
  gen_constraints start_var ((f, t) `HasFieldT` rest) a

gen_constraints start_var (RecordT ((f, t) `ConsRT` rest)) a =
  gen_constraints_assert_has_field start_var f t a ++ gen_constraints start_var (RecordT rest) a
gen_constraints start_var a (RecordT ((f, t) `ConsRT` rest)) =
  gen_constraints start_var (RecordT ((f, t) `ConsRT` rest)) a
gen_constraints start_var (RecordT NilRT) arg = []

gen_constraints start_var (a :-> b) arg = case arg of
  a' :-> b' -> gen_constraints start_var a a' ++ gen_constraints start_var b b'
  _ -> []
gen_constraints start_var (IntT) arg = []
gen_constraints start_var (VoidT) arg = []
gen_constraints start_var (BoolT) arg = []
gen_constraints start_var (MaybeT t) arg = case arg of
  (MaybeT t') -> gen_constraints start_var t t'
  _ -> []
gen_constraints start_var (EitherT l r) arg = case arg of
  (EitherT l' r') -> gen_constraints start_var l l' ++ gen_constraints start_var r r'
  _ -> []
gen_constraints start_var (CharT) arg = []
gen_constraints start_var (ListT t) arg = case arg of
  (ListT t') -> gen_constraints start_var t t'
  _ -> []
gen_constraints start_var (IO_T t) arg = case arg of
  (IO_T t') -> gen_constraints start_var t t'
  _ -> []
gen_constraints start_var (DynamicT) arg = []
gen_constraints start_var (UnitT) arg = []
gen_constraints start_var (PairT a b) arg = case arg of
  PairT a' b' -> gen_constraints start_var a a' ++ gen_constraints start_var b b'
  _ -> []
gen_constraints start_var (TripleT a b c) arg = case arg of
  TripleT a' b' c' -> gen_constraints start_var a a' ++ gen_constraints start_var b b' ++ gen_constraints start_var c c'
  _ -> []

var_to_type :: Mono TypeVar -> Mono Type
var_to_type (Mono a) = Mono $ TypeVarT a

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

unify :: forall t u. (T t ~ Type, T u ~ Type, Show (Mono t)) =>
  (forall a. A Type a => t a -> Mono Type) -> Poly t ->
  (forall a. A Type a => t a -> Mono Type) -> Poly t ->
  (forall a b. (A Type a, A Type b) => t a -> t b -> Poly u) -> Poly u
unify f_a a_input f_b b_input cont =
  let (a_poly, b_poly) = synchronize_quantifiers a_input b_input in
  let (a_mono, b_mono, m, start_var) = unpack_poly a_poly b_poly in
  let
    constraints =
      --trace (show (show m, show start_var)) $
      --trace (show (show (typeof_polymap a_input), show (typeof_polymap b_input))) $
      --trace (show (show (typeof_polymap a_poly), show (typeof_polymap b_poly))) $
      --trace (show (show (a_poly), show (b_poly))) $
      case (a_mono, b_mono) of
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
{-
test_unify =
  let
    remove_digits :: String -> String
    remove_digits [] = []
    remove_digits (h:t) =
      let tt = remove_digits t in
      if any (== h) "0123456789" then tt else h:tt
  in
  let
    (e1, e2) =
      synchronize_quantifiers
        (polytype_examples !! 3)
        (polytype_examples !! 2)
  in (all id [
    remove_digits (show e1) == "forall a b c d e. (a -> b) -> (b -> a) -> a -> a",
    remove_digits (show e2) == "forall a b c d e. (c -> d) -> (d -> e) -> c -> e",
    remove_digits (show (unify
      (Mono . type_of) (polytype_examples !! 3)
      (Mono . type_of) (polytype_examples !! 2)
      (\a b -> MonoP $ Mono a)
    )) == "forall a b. (a -> b) -> (b -> a) -> a -> a",
    remove_digits (show (unify
      (Mono . type_of) (polytype_examples !! 3)
      (Mono . type_of) (polytype_examples !! 2)
      (\a b -> MonoP $ Mono b)
    )) == "forall a b. (a -> b) -> (b -> a) -> a -> a"
  ])
-}
