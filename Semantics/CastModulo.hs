{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.CastModulo (cast_modulo) where
import Base
import Unsafe.Coerce (unsafeCoerce)

cast_modulo :: forall a b e l. (A Type a, A Type b) => Ast l e a -> Maybe (Ast l e b)
cast_modulo x = if Mono (anything :: Type a) == Mono (anything :: Type b)
  then Just $ unsafeCoerce x
  else Nothing
