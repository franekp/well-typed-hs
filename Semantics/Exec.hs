{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Exec (uast_to_string) where

import Base

import Semantics.Typecheck (typecheck)
import Semantics.Eval (polyast_to_monoast)
import Semantics.Records (resolve_field_lookups)
import Semantics.Eval (eval_ast)

uast_to_string :: UAst Lo -> String
uast_to_string a = case (resolve_field_lookups . polyast_to_monoast . typecheck) a of
  Mono a -> show_value $ eval_ast a
