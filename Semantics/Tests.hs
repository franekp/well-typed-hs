{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Tests where
import Base
import qualified Base.Letters as Letters
import Semantics.Typecheck (typecheck, eval, typeof_polymap, eval_poly)
import Semantics.Unify (test_unify)

test_show_type = (map show polytype_examples == [
    "forall a1 b2. a -> (a -> b) -> b",
    "forall a3. (a -> a) -> (a -> a) -> a -> a",
    "forall a4 b5 c6. (a -> b) -> (b -> c) -> a -> c",
    "forall a7 b8. (a -> b) -> (b -> a) -> a -> a"
  ]) || (map show polytype_examples == [
    "forall a b. a -> (a -> b) -> b",
    "forall a. (a -> a) -> (a -> a) -> a -> a",
    "forall a b c. (a -> b) -> (b -> c) -> a -> c",
    "forall a b. (a -> b) -> (b -> a) -> a -> a"
  ])

remove_digits :: String -> String
remove_digits [] = []
remove_digits (h:t) =
  let tt = remove_digits t in
  if any (== h) "0123456789" then tt else h:tt

test_typecheck =
  let
    f uast =
      let ast = typecheck uast in
      (
        remove_digits $ show ast,
        remove_digits $ show $ typeof_polymap ast
      )
  in map f uast_func_examples == [(
      "forall a b. LambdaA (LambdaA (AppA VarA (LiftA VarA)))",
      "forall a b. a -> (a -> b) -> b"
    ),(
      "forall a b c. LambdaA (LambdaA (LambdaA (AppA (LiftA (LiftA VarA)) (AppA (LiftA VarA) VarA))))",
      "forall a b c. (a -> b) -> (c -> a) -> c -> b"
    ),(
      "forall a b c. LambdaA (LambdaA (LambdaA (AppA (LiftA VarA) (AppA (LiftA (LiftA VarA)) VarA))))",
      "forall a b c. (a -> b) -> (b -> c) -> a -> c"
    )]

test_eval =
  let f uast = (eval_poly $ typecheck uast :: Int) in
  map f uast_int_examples == [8, 8]

test_show_read_letter = all id [
    show (Mono Letters.A_LL) == "a",
    (read "a" :: Mono Letters.Letter) == Mono Letters.A_LL,
    show (Mono Letters.F_LL) == "f",
    (read "f" :: Mono Letters.Letter) == Mono Letters.F_LL,
    show (Mono Letters.A_UL) == "A",
    (read "A" :: Mono Letters.Letter) == Mono Letters.A_UL,
    show (Mono Letters.F_UL) == "F",
    (read "F" :: Mono Letters.Letter) == Mono Letters.F_UL
  ]

tests = all id [
    test_show_type, test_typecheck, test_eval, test_unify,
    test_show_read_letter
  ]