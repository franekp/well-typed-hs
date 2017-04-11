#include "extension_base.hs"
module DynTest where
import qualified Base

module_exports = Base.ExtModule [
    EXPORT("substract", NONE, (-) :: Int -> Int -> Int),
    EXPORT("|>", FORALL(a, b), flip ($) :: a -> (a -> b) -> b),
    EXPORT("$", FORALL(a, b), ($) :: (a -> b) -> a -> b),
    EXPORT("<|", FORALL(a, b), ($) :: (a -> b) -> a -> b)
  ]
