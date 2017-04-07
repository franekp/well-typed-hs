{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Tests where
import Base
import Semantics.Typecheck
import Semantics.Eval
import Semantics.Unify (test_unify)
import Semantics.Records (resolve_field_lookups)

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

test_typecheck_records =
  let
    f :: UAst -> String
    f uast = case resolve_field_lookups $ polyast_to_monoast $ typecheck uast of
        Mono ast -> show $ type_of ast
  in map f uast_record_examples == [
    "{A :: Int, b :: Int, fun :: Int -> Int}",
    "{nest :: {A :: Int, b :: Int, fun :: Int -> Int}, A :: Int}",
    "{::}", "Int", "Int", "Int", "Int"
  ]

test_eval =
  let
    f uast = (eval_monoast
      $ resolve_field_lookups
      $ polyast_to_monoast
      $ typecheck uast :: Int)
  in
  map f uast_int_examples == [8, 8]

test_eval_records =
  let
    f :: UAst -> String
    f uast = case resolve_field_lookups $ polyast_to_monoast $ typecheck uast of
        Mono ast -> show_value $ eval_ast ast
  in map f uast_record_examples == [
    "{A = 5, b = 2, fun = <func>}",
    "{nest = {A = 5, b = 2, fun = <func>}, A = 5}",
    "{}", "7", "7", "8", "7"
  ]

main =
  let
    f :: UAst -> String
    f uast = case resolve_field_lookups $ polyast_to_monoast $ typecheck uast of
        Mono ast -> (show_value $ eval_ast ast) ++ " :: " ++ (show $ type_of ast)
    f' uast = show $ typeof_polymap $ typecheck uast
  in print $ map f uast_record_examples

test_show_read_letter = all id [
    show (Mono A_LL) == "a",
    (read "a" :: Mono ChrRep) == Mono A_LL,
    show (Mono F_LL) == "f",
    (read "f" :: Mono ChrRep) == Mono F_LL,
    show (Mono A_UL) == "A",
    (read "A" :: Mono ChrRep) == Mono A_UL,
    show (Mono F_UL) == "F",
    (read "F" :: Mono ChrRep) == Mono F_UL
  ]

test_show_read_symbol = all id [
    show (Mono $ A_UL `ConsSYM` B_LL `ConsSYM` C_LL `ConsSYM` NilSYM) == "Abc",
    (read "Abc" :: Mono Symbol) == (Mono $ A_UL `ConsSYM` B_LL `ConsSYM` C_LL `ConsSYM` NilSYM)
  ]

test_show_read_fieldname = all id [
    show (Mono $ FieldName $ A_UL `ConsSYM` B_LL `ConsSYM` C_LL `ConsSYM` NilSYM) == "Abc",
    (read "Abc" :: Mono FieldName) == (Mono $ FieldName $ A_UL `ConsSYM` B_LL `ConsSYM` C_LL `ConsSYM` NilSYM)
  ]

test_show_record_type = map show monorecordtype_examples == [
    "{A :: Int, b :: Int, fun :: Int -> Int}",
    "{nest :: {A :: Int, b :: Int, fun :: Int -> Int}, A :: Int}",
    "{::}"
  ]

test_show_record = map show monorecord_examples == [
    "{A = 5, b = 2, fun = <func>}",
    "{nest = {A = 5, b = 2, fun = <func>}, A = 5}",
    "{}"
  ]

tests = all id [
    test_show_type, test_typecheck, test_typecheck_records,
    test_eval, test_eval_records, test_unify, test_show_read_letter,
    test_show_read_symbol, test_show_read_fieldname,
    test_show_record_type, test_show_record
  ]
