{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Records (resolve_field_lookups) where
import Base

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
    helper record field = case type_of record of
      RecordT (ConsRT (f', a') rest') ->
        let
          default' = helper (RecordTailA record) field
        in
        case cast f' of
          Just f' -> if f' == field then Mono $ RecordHeadA record else default'
          Nothing -> default'
