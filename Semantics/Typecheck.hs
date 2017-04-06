{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Typecheck (typecheck, typecheck') where
import Base
import Semantics.Unify (unify)
import Semantics.CastModulo (cast_modulo)
import Data.List (foldl')
import Data.Bits (xor)

newtype TypeEnv = TypeEnv [(String, Mono Type)]

lookup_type :: TypeEnv -> String -> Mono Type
lookup_type (TypeEnv []) var = error $ "unknown type variable: '" ++ var ++ "'"
lookup_type (TypeEnv ((name, tt):t)) var =
  if var == name then tt else lookup_type (TypeEnv t) var

update_typeenv :: TypeEnv -> String -> Mono Type -> TypeEnv
update_typeenv (TypeEnv li) name tt = TypeEnv $ (name, tt):li

typecheck_monotype :: TypeEnv -> UMonoType -> Mono Type
typecheck_monotype te IntUMT = Mono IntT
typecheck_monotype te (a `ArrowUMT` b) =
  case (typecheck_monotype te a, typecheck_monotype te b) of
    (Mono a', Mono b') -> Mono $ a' :-> b'
typecheck_monotype te (VarUMT var) = te `lookup_type` var

hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- FIXME: this should be a monad generating unique ids, not hashes!
typecheck_polytype :: forall u. T u ~ Type => TypeEnv -> UPolyType
  -> (forall a. A Type a => TypeEnv -> Type a -> Poly u) -> Poly u
typecheck_polytype te (MonoUPT monotype) cont =
  case typecheck_monotype te monotype of
    Mono a -> cont te a
typecheck_polytype te (ForallUPT var inner) cont = ForallP (hash var) (
    ExistsPoly $ typecheck_polytype
      (update_typeenv te var $ Mono (anything :: Type a)) inner cont
    :: forall a. A Type a => ExistsPoly u a)

lookup_var :: forall e l. Env e -> String -> Poly (Ast Hi e)
lookup_var NilEN var = MonoP $ Mono
  (ErrorA $ "unknown variable: '" ++ var ++ "'" :: Ast Hi e Void)
lookup_var ((name, ty) `ConsEN` rest) var =
  if var == name then
    MonoP $ Mono VarA
  else
    polymap (MonoP . Mono . LiftA) $ lookup_var rest var
lookup_var ((name, val) `LetEN` rest) var =
  if var == name then
    val
    -- FIXME: here should be generating fresh ids for quantifiers inside val
    -- so that stuff like "let id = \x -> x in id id" or
    -- "let apply = \a f -> f a in apply apply (\app -> app 3 unary_minus)"
    -- should work
  else
    lookup_var rest var

typecheck :: UAst -> Poly (Ast Hi '[])
typecheck = typecheck' (TypeEnv []) NilEN

typecheck' :: forall e. Typeable e => TypeEnv -> Env e -> UAst -> Poly (Ast Hi e)
typecheck' te e AddUA = MonoP $ Mono $ AddA
typecheck' te e (LiteralUA val) = MonoP $ Mono $ LiteralA val
typecheck' te e (AppUA fun arg) =
  unify type_of_arg (typecheck' te e fun) (Mono . type_of) (typecheck' te e arg) cont
  where
    type_of_arg :: A Type a => Ast Hi e a -> Mono Type
    type_of_arg fun = case type_of fun of
      a :-> b -> Mono a
      _ -> error "_ is not a function"
    cont :: (A Type a, A Type b) => Ast Hi e a -> Ast Hi e b -> Poly (Ast Hi e)
    cont fun arg = case type_of fun of
      a :-> b -> case cast_modulo arg of
        Just correct_arg -> MonoP $ Mono $ fun `AppA` correct_arg
        Nothing -> MonoP $ Mono $ (ErrorA "wrong type of function argument" :: Ast Hi e Void)
      _ -> MonoP $ Mono $ (ErrorA "_ is not a function" :: Ast Hi e Void)
typecheck' te e ((var_name, ty) `LambdaUA` body) = (typecheck_polytype te ty helper :: Poly (Ast Hi e)) where
  helper :: forall a l. A Type a => TypeEnv -> Type a -> Poly (Ast Hi e)
  helper te' tt = polymap (MonoP . Mono . (
      LambdaA :: forall b l. A Type b => Ast Hi (a ': e) b -> Ast Hi e (a -> b)
    )) body_ast
    where
      body_ast :: Poly (Ast Hi (a ': e))
      body_ast = typecheck' te' ((var_name, tt) `ConsEN` e) body
typecheck' te e (VarUA name) = lookup_var e name
typecheck' te e ((name, val) `LetUA` expr) =
  typecheck' te ((name, (typecheck' te e val)) `LetEN` e) expr
typecheck' te e RecordNilUA = MonoP $ Mono $ RecordNilA
typecheck' te e ((fu, au) `RecordConsUA` restu) = result where
  restp = typecheck' te e restu
  fm = (read fu :: Mono FieldName)
  ap = typecheck' te e au
  result = polymap cont_a ap
  cont_a :: forall a. A Type a => Ast Hi e a -> Poly (Ast Hi e)
  cont_a a = polymap cont_rest restp where
    cont_rest :: forall a. A Type a => Ast Hi e a -> Poly (Ast Hi e)
    cont_rest rest = case fm of
      Mono f -> case type_of rest of
        RecordT _ -> MonoP $ Mono $ (f, a) `RecordConsA` rest
        _ -> error "RecordCons with non-record tail."
typecheck' te e (RecordGetUA f r) = polymap cont $ typecheck' te e r where
  cont :: forall r. A Type r => Ast Hi e r -> Poly (Ast Hi e)
  cont r_ast = case type_of r_ast of
    HasFieldT ((f' :: FieldName f'), (_ :: Type a)) (_ :: Type r') ->
      case (read f :: Mono FieldName) of
        Mono (f :: FieldName f) -> case cast f of
          Just f ->
            if f == f' then
              MonoP $ Mono $ (RecordGetA f' :: Ast Hi e (HasField '(f', a) r') -> Ast Hi e a) r_ast
            else
              error "field name mismatch"
          Nothing ->
            error "field name mismatch"
    _ -> error "type mismatch"
