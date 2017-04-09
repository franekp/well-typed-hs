module DynTest where
#include "extension_base.hs"

module_exports = ExtModule [("substract", MonoP $ Mono $ Builtin ((-) :: Int -> Int -> Int))]
