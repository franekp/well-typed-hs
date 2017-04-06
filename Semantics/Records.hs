{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Records (resolve_field_lookups) where
import Base
import Semantics.CastModulo (cast_modulo)

resolve_field_lookups :: Mono (Ast Hi '[]) -> Mono (Ast Lo '[])
resolve_field_lookups (Mono a) = resolve_field_lookups' a

resolve_field_lookups' :: forall aa e. A Type aa => Ast Hi e aa -> Mono (Ast Lo e)
resolve_field_lookups' (RecordGetA field record) =
  case resolve_field_lookups' record of
    Mono record' -> case type_of record' of
      RecordT _ -> helper record' field
      _ -> error "not a record type"
  where
    helper :: forall f r. (A RecordType r, Typeable f) => Ast Lo e (Record r) -> FieldName f -> Mono (Ast Lo e)
    helper record field = case (type_of record :: Type (Record r)) of
      RecordT (ConsRT (f', a') rest') ->
        let
          default' = helper (RecordTailA record) field
        in
        case cast f' of
          Just f' -> if f' == field then Mono $ RecordHeadA record else default'
          Nothing -> default'
      RecordT NilRT -> error "no such field"
resolve_field_lookups' (a@AddA) = Mono $ helper a where
  helper :: forall e. Ast Hi e (Int -> Int -> Int) -> Ast Lo e (Int -> Int -> Int)
  helper _ = AddA
resolve_field_lookups' (a@(LiteralA i)) = Mono $ helper a where
  helper :: forall e. Ast Hi e Int -> Ast Lo e Int
  helper (LiteralA i) = LiteralA i
  helper _ = error "unreachable"
resolve_field_lookups' (a@VarA) = Mono $ helper a where
  helper :: forall a e. A Type a => Ast Hi (a ': e) a -> Ast Lo (a ': e) a
  helper _ = VarA
resolve_field_lookups' (a@(LiftA x)) = helper a where
  helper :: forall e a b. (A Type a, A Type b) => Ast Hi (b ': e) a -> Mono (Ast Lo (b ': e))
  helper (LiftA x) = case resolve_field_lookups' x of
    Mono x -> Mono $ LiftA x
  helper _ = error "unreachable"
resolve_field_lookups' (a@(LambdaA x)) = helper a where
  helper :: forall e a b. (A Type a, A Type b) => Ast Hi e (a -> b) -> Mono (Ast Lo e)
  helper (LambdaA x) = case resolve_field_lookups' x of
    Mono x -> Mono $ LambdaA x
  helper _ = error "unreachable"
resolve_field_lookups' (a@(ErrorA msg)) = helper a where
  helper :: forall e a. A Type a => Ast Hi e a -> Mono (Ast Lo e)
  helper (ErrorA msg) = Mono $ (ErrorA msg :: Ast Lo e a)
  helper _ = error "unreachable"
resolve_field_lookups' (AppA f a) = result where
  f' = resolve_field_lookups' f
  a' = resolve_field_lookups' a
  cont :: forall a b. (A Type a, A Type b) => Ast Lo e a -> Ast Lo e b -> Mono (Ast Lo e)
  cont fun arg = case type_of fun of
    a :-> b -> case cast_modulo arg of
      Just correct_arg -> Mono $ fun `AppA` correct_arg
      Nothing -> Mono $ (ErrorA "wrong type of function argument" :: Ast Lo e Void)
    _ -> Mono $ (ErrorA "_ is not a function" :: Ast Lo e Void)
  result = case f' of
    Mono ff -> case a' of
      Mono aa -> cont ff aa
resolve_field_lookups' (a@(RecordHeadA x)) = helper a where
  helper :: forall e a. A Type a => Ast Hi e a -> Mono (Ast Lo e)
  helper (RecordHeadA x) = case resolve_field_lookups' x of
    Mono x -> case type_of x of
      RecordT (ConsRT (f, a) r) -> Mono $ RecordHeadA x
      _ -> error "unreachable"
  helper _ = error "unreachable"
resolve_field_lookups' (a@(RecordTailA x)) = helper a where
  helper :: forall e r. A RecordType r => Ast Hi e (Record r) -> Mono (Ast Lo e)
  helper (RecordTailA x) = case resolve_field_lookups' x of
    Mono x -> case type_of x of
      RecordT (ConsRT (f, a) r) -> Mono $ RecordTailA x
      _ -> error "unreachable"
  helper _ = error "unreachable"
resolve_field_lookups' (a@RecordNilA) = Mono $ helper a where
  helper :: forall e. Ast Hi e (Record '[]) -> Ast Lo e (Record '[])
  helper _ = RecordNilA
resolve_field_lookups' (RecordConsA (f, a) r) = result where
  helper :: forall e a f r. (A Type a, A FieldName f, A RecordType r) =>
    (FieldName f, Ast Lo e a) -> Ast Lo e (Record r) -> Mono (Ast Lo e)
  helper (f', a') r' =
    Mono $ RecordConsA (f', a') r'
  result = case resolve_field_lookups' a of
    Mono a' -> case resolve_field_lookups' r of
      Mono r' -> case type_of r' of
        RecordT _ -> helper (f, a') r'
        _ -> error "unreachable"
