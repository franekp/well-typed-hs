{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Exec (uast_to_string, uast_to_io) where

import Base

import Semantics.Typecheck (typecheck)
import Semantics.Eval (polyast_to_monoast)
import Semantics.Eval (eval_ast, eval_monoast)

uast_to_string :: ExtModuleEnv -> UAst -> String
uast_to_string me a = case (polyast_to_monoast . typecheck me) a of
  Mono a -> show_value $ eval_ast a

uast_to_io :: ExtModuleEnv -> UAst -> IO ()
uast_to_io me = eval_monoast . polyast_to_monoast . typecheck me
