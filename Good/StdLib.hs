#include "extension_base.hs"
module StdLib where
import qualified Base

module_exports = Base.ExtModule [
    EXPORT("-", NONE, (-) :: Int -> Int -> Int),
    EXPORT("+", NONE, (+) :: Int -> Int -> Int),
    EXPORT("*", NONE, (*) :: Int -> Int -> Int),
    EXPORT("/", NONE, div :: Int -> Int -> Int),
    EXPORT("**", NONE, (^) :: Int -> Int -> Int),
    EXPORT("|>", FORALL(a, b), flip ($) :: a -> (a -> b) -> b),
    EXPORT("$", FORALL(a, b), ($) :: (a -> b) -> a -> b),
    EXPORT("<|", FORALL(a, b), ($) :: (a -> b) -> a -> b)
  ]
