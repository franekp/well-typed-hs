{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Exec (uast_to_string, uast_to_io) where

import Base

import Semantics.Typecheck (typecheck)
import Semantics.Eval (polyast_to_monoast)
import Semantics.Records (resolve_field_lookups)
import Semantics.Eval (eval_ast, eval_monoast)

uast_to_string :: ExtModuleEnv -> UAst Lo -> String
uast_to_string me a = case (resolve_field_lookups . polyast_to_monoast . typecheck me) a of
  Mono a -> show_value $ eval_ast a

uast_to_io :: ExtModuleEnv -> UAst Lo -> IO ()
uast_to_io me = eval_monoast . resolve_field_lookups . polyast_to_monoast . typecheck me
