{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.Typecheck (typecheck, typecheck') where
import Base
import Semantics.Unify (unify)
import Semantics.CastModulo (cast_modulo)
import Data.List (foldl')
import Data.Bits (xor)

import GHC.Stack (errorWithStackTrace)

newtype TypeEnv = TypeEnv [(String, Mono Type)]

lookup_type :: SourceInfo -> TypeEnv -> String -> Mono Type
lookup_type src (TypeEnv []) var = error $ "Undefined type\n\n" ++ show_source src
lookup_type src (TypeEnv ((name, tt):t)) var =
  if var == name then tt else lookup_type src (TypeEnv t) var

update_typeenv :: TypeEnv -> String -> Mono Type -> TypeEnv
update_typeenv (TypeEnv li) name tt = TypeEnv $ (name, tt):li

lookup_ext_module :: ExtModuleEnv -> String -> ExtModule
lookup_ext_module (ExtModuleEnv []) var = error $ "unknown module: '" ++ var ++ "'"
lookup_ext_module (ExtModuleEnv ((name, mo):rest)) var =
  if var == name then mo else lookup_ext_module (ExtModuleEnv rest) var

typecheck_monotype :: TypeEnv -> UMonoType -> Mono Type
typecheck_monotype te (UMonoType src (a `ArrowUMT` b)) =
  case (typecheck_monotype te a, typecheck_monotype te b) of
    (Mono a', Mono b') -> Mono $ a' :-> b'
typecheck_monotype te (UMonoType src (VarUMT var)) = lookup_type src te var
typecheck_monotype te (UMonoType src (HasFieldUMT (field, a) rest)) =
  case (typecheck_monotype te a, typecheck_monotype te rest) of
    (Mono a', Mono rest') -> case (read field :: Mono FieldName) of
      Mono (field' :: FieldName f) -> Mono $ HasFieldT (field', a') rest'
typecheck_monotype te (UMonoType src (RecordConsUMT (field, a) rest)) =
  case (typecheck_monotype te a, typecheck_monotype te rest) of
    (Mono a', Mono (RecordT rest')) -> case (read field :: Mono FieldName) of
      Mono (field' :: FieldName f) -> Mono $ RecordT $ ConsRT (field', a') rest'
    (_, Mono _) -> error "this code should be unreachable"
typecheck_monotype te (UMonoType src RecordNilUMT) = Mono $ RecordT $ NilRT

typecheck_monotype te (UMonoType src IntUMT) = Mono IntT
typecheck_monotype te (UMonoType src BoolUMT) = Mono BoolT
typecheck_monotype te (UMonoType src (MaybeUMT a)) =
  case typecheck_monotype te a of
    Mono a' -> Mono $ MaybeT a'
typecheck_monotype te (UMonoType src (EitherUMT a b)) =
  case (typecheck_monotype te a, typecheck_monotype te b) of
    (Mono a', Mono b') -> Mono $ EitherT a' b'
typecheck_monotype te (UMonoType src CharUMT) = Mono CharT
typecheck_monotype te (UMonoType src (ListUMT a)) =
  case typecheck_monotype te a of
    Mono a' -> Mono $ ListT a'
typecheck_monotype te (UMonoType src (IO_UMT a)) =
  case typecheck_monotype te a of
    Mono a' -> Mono $ IO_T a'
typecheck_monotype te (UMonoType src DynamicUMT) = Mono DynamicT
typecheck_monotype te (UMonoType src UnitUMT) = Mono UnitT
typecheck_monotype te (UMonoType src (PairUMT a b)) =
  case (typecheck_monotype te a, typecheck_monotype te b) of
    (Mono a', Mono b') -> Mono $ PairT a' b'
typecheck_monotype te (UMonoType src (TripleUMT a b c)) =
  case (typecheck_monotype te a, typecheck_monotype te b, typecheck_monotype te c) of
    (Mono a', Mono b', Mono c') -> Mono $ TripleT a' b' c'

hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- FIXME: this should be a monad generating unique ids, not hashes!
typecheck_polytype :: forall u. T u ~ Type => TypeEnv -> UPolyType
  -> (forall a. A Type a => TypeEnv -> Type a -> Poly u) -> Poly u
typecheck_polytype te (UPolyType src (MonoUPT monotype)) cont =
  case typecheck_monotype te monotype of
    Mono a -> cont te a
typecheck_polytype te (UPolyType src (ForallUPT var inner)) cont = ForallP (hash var) (
    ExistsPoly $ typecheck_polytype
      (update_typeenv te var $ Mono (anything :: Type a)) inner cont
    :: forall a. A Type a => ExistsPoly u a)

lookup_var :: forall e l. SourceInfo -> Env e -> String -> Poly (Ast Hi e)
lookup_var src NilEN var = error $ "Undefined variable '" ++ var ++ "'\n\n" ++ show_source src
lookup_var src ((name, ty) `ConsEN` rest) var =
  if var == "+" then MonoP $ Mono AddA else
  if var == name then
    MonoP $ Mono VarA
  else
    polymap (MonoP . Mono . LiftA) $ lookup_var src rest var
lookup_var src ((name, val) `LetEN` rest) var =
  if var == name then
    val
    -- FIXME: here should be generating fresh ids for quantifiers inside val
    -- so that stuff like "let id = \x -> x in id id" or
    -- "let apply = \a f -> f a in apply apply (\app -> app 3 unary_minus)"
    -- should work
  else
    lookup_var src rest var
lookup_var src (ExtModule [] `OpenEN` rest) var = lookup_var src rest var
lookup_var src (ExtModule ((name, builtin):t) `OpenEN` rest) var =
  if name == var then
    polymap (MonoP . Mono . BuiltinA) builtin
  else
    lookup_var src (ExtModule t `OpenEN` rest) var

typecheck :: ExtModuleEnv -> UAst Lo -> Poly (Ast Hi '[])
typecheck me = typecheck' me (TypeEnv []) NilEN

typecheck' :: forall e. ExtModuleEnv -> TypeEnv -> Env e -> UAst Lo -> Poly (Ast Hi e)
typecheck' me te e (UAst src AddUA) = MonoP $ Mono $ AddA
typecheck' me te e (UAst src (LiteralUA val)) = MonoP $ Mono $ LiteralA val
typecheck' me te e (UAst src (StringUA s)) = MonoP $ Mono $ BuiltinA $ Builtin s
typecheck' me te e (UAst src (AppUA fun' arg')) =
  unify type_of_arg (typecheck' me te e fun') (Mono . type_of) (typecheck' me te e arg') cont
  where
    type_of_arg :: A Type a => Ast Hi e a -> Mono Type
    type_of_arg fun = case type_of fun of
      a :-> b -> Mono a
      _ ->
        error $ "Cannot call a non-function\n\n"
        ++ show fun' ++ "\nType: " ++ show (type_of fun) ++ "\n"
    cont :: (A Type a, A Type b) => Ast Hi e a -> Ast Hi e b -> Poly (Ast Hi e)
    cont fun arg = case type_of fun of
      a :-> b -> case cast_modulo arg of
        Just correct_arg -> MonoP $ Mono $ fun `AppA` correct_arg
        Nothing -> error $ "Type mismatch\n\n"
          ++ show fun' ++ "\nFunction type: " ++ show (type_of fun)
          ++"\n\n" ++ show arg' ++ "\nArgument type: " ++ show (type_of arg) ++ "\n"
      _ ->
        error $ "Cannot call a non-function\n\n"
        ++ show fun' ++ "\nType: " ++ show (type_of fun) ++ "\n"
typecheck' me te e (UAst src ((var_name, ty) `LambdaUA` body)) =
  (typecheck_polytype te ty helper :: Poly (Ast Hi e)) where
    helper :: forall a l. A Type a => TypeEnv -> Type a -> Poly (Ast Hi e)
    helper te' tt = polymap (MonoP . Mono . (
        LambdaA :: forall b l. A Type b => Ast Hi (a ': e) b -> Ast Hi e (a -> b)
      )) body_ast
      where
        body_ast :: Poly (Ast Hi (a ': e))
        body_ast = typecheck' me te' ((var_name, tt) `ConsEN` e) body
typecheck' me te e (UAst src (VarUA name)) = lookup_var src e name
typecheck' me te e (UAst src ((name, val) `LetUA` expr)) =
  typecheck' me te ((name, (typecheck' me te e val)) `LetEN` e) expr
typecheck' me te e (UAst src RecordNilUA) = MonoP $ Mono $ RecordNilA
typecheck' me te e (UAst src ((fu, au) `RecordConsUA` restu)) = result where
  restp = typecheck' me te e restu
  fm = (read fu :: Mono FieldName)
  ap = typecheck' me te e au
  result = polymap cont_a ap
  cont_a :: forall a. A Type a => Ast Hi e a -> Poly (Ast Hi e)
  cont_a a = polymap cont_rest restp where
    cont_rest :: forall a. A Type a => Ast Hi e a -> Poly (Ast Hi e)
    cont_rest rest = case fm of
      Mono f -> case type_of rest of
        RecordT _ -> MonoP $ Mono $ (f, a) `RecordConsA` rest
        _ -> error "RecordCons with non-record tail."
typecheck' me te e (UAst src (RecordGetUA f r)) = polymap cont $ typecheck' me te e r where
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
            error $ "field name mismatch: " ++ show f ++ " != " ++ show f'
    _ -> error $ "type mismatch (record-related)\n\n" ++ show_source src ++ "\n" ++ show (type_of r_ast)
typecheck' me te e (UAst src (OpenUA mod rest)) =
  typecheck' me te (OpenEN (lookup_ext_module me mod) e) rest
typecheck' me (TypeEnv te) e (UAst src (TypeDefUA (name, tt) expr)) =
  let ty = typecheck_monotype (TypeEnv te) tt in
  typecheck' me (TypeEnv $ (name, ty):te) e expr
