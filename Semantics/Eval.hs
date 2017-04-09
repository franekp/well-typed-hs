{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Eval where
import Base
import Semantics.CastModulo (cast_modulo)

eval_ast :: Ast Lo '[] a -> a
eval_ast = eval_ast' NilST

eval_ast' :: forall a e. Store e -> Ast Lo e a -> a
eval_ast' s AddA = \x y -> x + y
eval_ast' s (LiteralA x) = x
eval_ast' s VarA =
  let
    helper :: forall a e. Store (a ': e) -> a
    helper (val `ConsST` _) = val  -- workaround exhaustiveness check that sucks
  in helper s
eval_ast' s (LiftA a) = let
    helper :: forall a e. Store (a ': e) -> Store e
    helper (_ `ConsST` s') = s'  -- workaround exhaustiveness check that sucks
  in eval_ast' (helper s) a
eval_ast' s (LambdaA body) = \arg -> eval_ast' (arg `ConsST` s) body
eval_ast' s (ErrorA msg) = error msg
eval_ast' s (AppA fun arg) = eval_ast' s fun $ eval_ast' s arg
eval_ast' s (RecordHeadA r) = case eval_ast' s r of
  (name, val) `ConsRC` rest -> val
eval_ast' s (RecordTailA r) = case eval_ast' s r of
  (name, val) `ConsRC` rest -> rest
eval_ast' s RecordNilA = NilRC
eval_ast' s ((f, h) `RecordConsA` t) = (f, eval_ast' s h) `ConsRC` eval_ast' s t
eval_ast' s (BuiltinA (Builtin a)) = a

eval_monoast :: A Type a => Mono (Ast Lo '[]) -> a
eval_monoast = eval_ast . forcetype_monoast

polyast_to_monoast :: Poly (Ast Hi '[]) -> Mono (Ast Hi '[])
polyast_to_monoast (ForallP _ exists_poly) =
  case exists_poly of
    (ExistsPoly poly :: ExistsPoly (Ast Hi '[]) Void) ->
      polyast_to_monoast poly
polyast_to_monoast (MonoP res) = res

forcetype_monoast :: A Type a => Mono (Ast Lo '[]) -> Ast Lo '[] a
forcetype_monoast (Mono ast) = case cast_modulo ast of
  Just x -> x
  Nothing -> ErrorA $ "wrong type of: " ++ show ast
