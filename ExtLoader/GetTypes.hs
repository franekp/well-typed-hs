#include "../settings.hs"

module ExtLoader.GetTypes (get_ext_module_types) where

import Base

unpack_poly :: Poly Type -> (Mono Type, [Mono TypeVar])
unpack_poly a = unpack_poly' ZeroTV a []
  where
    unpack_poly' :: forall a. A TypeVar a => TypeVar a -> Poly Type
      -> [Mono TypeVar] -> (Mono Type, [Mono TypeVar])
    unpack_poly' last_tv (MonoP a) m = (a, m)
    unpack_poly' last_tv
      (ForallP num_a (ExistsPoly poly_a :: ExistsPoly Type (RuntimeTypeVar a))) m =
        unpack_poly' (SuccTV last_tv) poly_a $ Mono last_tv : m
    unpack_poly' _ _ _ = error "unreachable"

xx = UMonoType ((-1, -1), (-1, -1), ("", ""))

trans_monotype :: Mono Type -> UMonoType
trans_monotype (Mono a) = trans_type a

trans_type :: A Type a => Type a -> UMonoType
trans_type = xx . \case
  (a :-> b) -> trans_type a `ArrowUMT` trans_type b
  IntT -> IntUMT
  VoidT -> error "Void"
  TypeVarT a -> VarUMT $ show a
  RecordT ((f, a) `ConsRT` rest) -> (show f, trans_type a) `RecordConsUMT` trans_type (RecordT rest)
  RecordT NilRT -> RecordNilUMT
  (f, a) `HasFieldT` rest -> (show f, trans_type a) `RecordConsUMT` trans_type rest

  BoolT -> BoolUMT
  MaybeT t -> MaybeUMT $ trans_type t
  EitherT l r -> EitherUMT (trans_type l) (trans_type r)
  CharT -> CharUMT
  ListT t -> ListUMT $ trans_type t
  IO_T t -> IO_UMT $ trans_type t
  DynamicT -> DynamicUMT
  UnitT -> UnitUMT
  PairT l r -> PairUMT (trans_type l) (trans_type r)
  TripleT a b c -> TripleUMT (trans_type a) (trans_type b) (trans_type c)

trans_polytype :: Poly Type -> UPolyType
trans_polytype polytype = UPolyType (map show vars) $ trans_monotype tt where
  (tt, vars) = unpack_poly polytype

get_ext_module_types :: ExtModuleEnv -> ExtModuleTypeEnv
get_ext_module_types (ExtModuleEnv e) = ExtModuleTypeEnv $ map get_types_module e where
  get_types_module :: (String, ExtModule) -> (String, [(String, UPolyType)])
  get_types_module (name, ExtModule bindings) = (name, map get_types_binding bindings)
  get_types_binding :: (String, Poly Builtin) -> (String, UPolyType)
  get_types_binding (s, b) = (s, trans_polytype $ polymap (MonoP . Mono . type_of) b)
