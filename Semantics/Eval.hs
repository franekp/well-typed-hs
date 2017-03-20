{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Eval where
import Base

eval :: Ast l '[] a -> a
eval = eval' NilST

eval' :: forall a e l. Store e -> Ast l e a -> a
eval' s AddA = \x y -> x + y
eval' s (LiteralA x) = x
eval' s VarA =
  let
    helper :: forall a e. Store (a ': e) -> a
    helper (val `ConsST` _) = val  -- workaround exhaustiveness check that sucks
  in helper s
eval' s (LiftA a) = let
    helper :: forall a e. Store (a ': e) -> Store e
    helper (_ `ConsST` s') = s'  -- workaround exhaustiveness check that sucks
  in eval' (helper s) a
eval' s (LambdaA body) = \arg -> eval' (arg `ConsST` s) body
eval' s (ErrorA msg) = error msg
eval' s (AppA fun arg) = eval' s fun $ eval' s arg
eval' s (RecordHeadA r) = case eval' s r of
  (name, val) `ConsRC` rest -> val
eval' s (RecordTailA r) = case eval' s r of
  (name, val) `ConsRC` rest -> rest
eval' s RecordNilA = NilRC
eval' s ((f, h) `RecordConsA` t) = (f, eval' s h) `ConsRC` eval' s t

eval_poly :: A Type a => Poly (Ast Hi '[]) -> a
eval_poly = eval . forcetype

makemono :: Poly (Ast l '[]) -> Mono (Ast l '[])
makemono (ForallP _ exists_poly) =
  case exists_poly of
    (ExistsPoly poly :: ExistsPoly (Ast l '[]) Void) -> makemono poly
makemono (MonoP res) = res

forcetype :: A Type a => Poly (Ast Hi '[]) -> Ast Hi '[] a
forcetype (ForallP _ exists_poly) =
  case exists_poly of
    (ExistsPoly poly :: ExistsPoly (Ast Hi '[]) Void) -> forcetype poly
forcetype (MonoP (Mono ast)) = case cast ast of
  Just x -> x
  Nothing -> ErrorA $ "wrong type of: " ++ show ast

typeof_polymap :: Poly (Ast Hi '[]) -> Poly Type
typeof_polymap = polymap (MonoP . Mono . type_of)
