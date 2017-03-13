{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Typecheck (typecheck, typecheck') where
import Base
import Semantics.Unify (unify)
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

lookup_var :: forall e. Env e -> String -> Poly (Ast e)
lookup_var NilEN var = MonoP $ Mono
  (ErrorA $ "unknown variable: '" ++ var ++ "'" :: Ast e Void)
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

typecheck :: UAst -> Poly (Ast '[])
typecheck = typecheck' (TypeEnv []) NilEN

typecheck' :: forall e. Typeable e => TypeEnv -> Env e -> UAst -> Poly (Ast e)
typecheck' te e AddUA = MonoP $ Mono $ AddA
typecheck' te e (LiteralUA val) = MonoP $ Mono $ LiteralA val
typecheck' te e (AppUA fun arg) =
  unify type_of_arg (typecheck' te e fun) (Mono . type_of) (typecheck' te e arg) cont
  where
    type_of_arg :: A Type a => Ast e a -> Mono Type
    type_of_arg fun = case type_of fun of
      a :-> b -> Mono a
      _ -> error "_ is not a function"
    cont :: (A Type a, A Type b) => Ast e a -> Ast e b -> Poly (Ast e)
    cont fun arg = case type_of fun of
      a :-> b -> case cast arg of
        Just correct_arg -> MonoP $ Mono $ fun `AppA` correct_arg
        Nothing -> MonoP $ Mono $ (ErrorA "wrong type of function argument" :: Ast e Void)
      _ -> MonoP $ Mono $ (ErrorA "_ is not a function" :: Ast e Void)
typecheck' te e ((var_name, ty) `LambdaUA` body) = (typecheck_polytype te ty helper :: Poly (Ast e)) where
  helper :: forall a. A Type a => TypeEnv -> Type a -> Poly (Ast e)
  helper te' tt = polymap (MonoP . Mono . (
      LambdaA :: forall b. A Type b => Ast (a ': e) b -> Ast e (a -> b)
    )) body_ast
    where
      body_ast :: Poly (Ast (a ': e))
      body_ast = typecheck' te' ((var_name, tt) `ConsEN` e) body
typecheck' te e (VarUA name) = lookup_var e name
typecheck' te e ((name, val) `LetUA` expr) =
  typecheck' te ((name, (typecheck' te e val)) `LetEN` e) expr
