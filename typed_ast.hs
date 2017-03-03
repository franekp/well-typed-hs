{- LANGUAGE GADTs, OverlappingInstances, IncoherentInstances, TypeFamilies, DeriveFunctor, ViewPatterns, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             GADTs,
             RankNTypes,
             StandaloneDeriving,
             DeriveDataTypeable,
             ScopedTypeVariables,
             FunctionalDependencies,
             OverlappingInstances,
             FlexibleInstances,
             FlexibleContexts,
             ExistentialQuantification,
             UndecidableInstances,
             TypeFamilies #-}

import Data.Void
import Data.Typeable

data UntypedAst = AddU | MultU | NegateU
  | LiteralU Int
  | AppU UntypedAst UntypedAst
  | LambdaU String UntypedType UntypedAst
  | VarU String
  | IdU | ComposeU | ApplyU | Choose2U

data UntypedType = IntUT | ArrowUT UntypedType UntypedType

data Type a where
  ArrowTT :: (Any a, Any b) => Type a -> Type b -> Type (a -> b)
  IntTT :: Type Int
  VoidTT :: Type Void

data TypeLocation = HereTL | LeftTL TypeLocation | RightTL TypeLocation | BothTL TypeLocation TypeLocation

deriving instance Show (Type a)

data OpaqueType where
  OpaqueType :: forall a. Any a => Type a -> OpaqueType

type Store = (forall a. Any a => String -> Type a -> Maybe a)

data Ast a where
  AddT :: Ast (Int -> Int -> Int)
  MultT :: Ast (Int -> Int -> Int)
  NegateT :: Ast (Int -> Int)
  LiteralT :: Int -> Ast Int
  ErrorT :: Any a => String -> Ast a
  AppT :: (Any a, Any b) => Ast (a -> b) -> Ast a -> Ast b
  VarT :: Any a => String -> Type a -> Ast a
  LambdaT :: (Any a, Any b) => String -> Type a -> Ast b -> Ast (a -> b)
  IdT :: Any a => Ast (a -> a)
  ComposeT :: Any a => Ast ((a -> a) -> (a -> a) -> (a -> a))
  ApplyT :: (Any a, Any b) => Ast (a -> (a -> b) -> b)
  Choose2T :: Any a => Ast (Int -> a -> a -> a)

deriving instance Typeable1 Ast
deriving instance Show (Ast a)
deriving instance Show TypeLocation

class (Typeable a) => Any a where
  get_type :: Any a => Type a

instance Any Int where
  get_type = IntTT

instance Any Void where
  get_type = VoidTT

instance (Any a, Any b) => Any (a -> b) where
  get_type = ArrowTT (get_type :: Type a) (get_type :: Type b)

type_of :: Any a => Ast a -> Type a
type_of a = get_type

make_app_mono_mono :: (Any a, Any b) => Ast a -> Ast b -> OpaqueAst
make_app_mono_mono fun arg = case type_of fun of
  IntTT -> case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "Int is not a function!" :: Ast Void)
  VoidTT -> case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "Void is not a function!" :: Ast Void)
  ArrowTT a b -> case cast arg of
    Just argg -> OpaqueAst $ AppT fun argg
    Nothing -> OpaqueAst $ (ErrorT $
      ("Type mismatch: trying to apply " ++ (show $ type_of fun) ++ " to "
      ++ (show $ type_of arg)) :: Ast Void)

polyast_error :: String -> PolyAst Void
polyast_error a = PolyAst $ OpaqueAst $ (ErrorT a :: Ast Void)

elim_one_forall_with_lookup_in_monotype :: forall res a. Any a =>
  TypeLocation -> (forall x. Any x => PolyAst x) -> Type a
  -> (forall y. Any y => PolyAst y -> res) -> res
elim_one_forall_with_lookup_in_monotype HereTL phe tt cont = make_result phe tt cont where
  make_result :: Any x => PolyAst x -> Type x -> (forall y. Any y => PolyAst y -> res) -> res
  make_result phe_ tt_ cont_ = cont_ phe_
elim_one_forall_with_lookup_in_monotype (LeftTL _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
elim_one_forall_with_lookup_in_monotype (RightTL _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
elim_one_forall_with_lookup_in_monotype (BothTL _ _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
elim_one_forall_with_lookup_in_monotype (LeftTL left) phe (ArrowTT l r) cont = elim_one_forall_with_lookup_in_monotype left phe l cont
elim_one_forall_with_lookup_in_monotype (RightTL right) phe (ArrowTT l r) cont = elim_one_forall_with_lookup_in_monotype right phe r cont
elim_one_forall_with_lookup_in_monotype (BothTL right left) phe (ArrowTT l r) cont = elim_one_forall_with_lookup_in_monotype left phe l cont  -- ???

make_app_poly_mono :: Any a => TypeLocation -> (forall x. Any x => PolyAst x) -> Ast a -> OpaqueAst
make_app_poly_mono (LeftTL left) phe arg = elim_one_forall_with_lookup_in_monotype left phe (type_of arg) do_stuff where
  do_stuff :: PolyAst x -> OpaqueAst
  do_stuff (PolyAst (OpaqueAst fun)) = make_app_mono_mono fun arg
  do_stuff (PolyAst (OpaqueAstPoly loc newphe)) = make_app_poly_mono loc newphe arg
make_app_poly_mono (BothTL left right) phe arg = elim_one_forall_with_lookup_in_monotype left phe (type_of arg) do_stuff where
  do_stuff :: PolyAst x -> OpaqueAst
  do_stuff (PolyAst (OpaqueAst fun)) = make_app_mono_mono fun arg
  do_stuff (PolyAst (OpaqueAstPoly loc newphe)) = make_app_poly_mono loc newphe arg
make_app_poly_mono (RightTL right) phe arg = OpaqueAstPoly right $ do_stuff phe arg where
  -- this is the true type for that, but it did not worked - this more specific type is automagically generalized somehow
  -- and conveys information that each x from the left corresponds to x from the right - the type variable stays the same
  -- do_stuff :: Any aa => (forall x. Any x => PolyAst x) -> Ast aa -> (forall y. Any y => PolyAst y)
  do_stuff :: (Any aa, Any x) => PolyAst x -> Ast aa -> PolyAst x
  do_stuff (PolyAst fun) arg = PolyAst $ make_app fun (OpaqueAst arg)
make_app_poly_mono HereTL phe arg = OpaqueAst $ (ErrorT $ "Trying to use variable of generic type as a function with argument: " ++ show arg :: Ast Void)

make_app :: OpaqueAst -> OpaqueAst -> OpaqueAst
make_app (OpaqueAst func) (OpaqueAst a) = make_app_mono_mono func a
make_app (OpaqueAstPoly loc polyhelperexists) (OpaqueAst arg) =
 make_app_poly_mono loc polyhelperexists arg

data PolyAst x where
  PolyAst :: Any x => OpaqueAst -> PolyAst x

data OpaqueAst where
  OpaqueAst :: Any a => Ast a -> OpaqueAst
  OpaqueAstPoly :: TypeLocation -> (forall x. Any x => PolyAst x) -> OpaqueAst

instance Show OpaqueAst where
  show (OpaqueAst a) = show a
  show (OpaqueAstPoly tl polyast) = case (polyast :: PolyAst Void) of
    PolyAst opaque_ast -> "[" ++ show tl ++ "] " ++ show opaque_ast

typecheck_ast :: UntypedAst -> OpaqueAst
typecheck_type :: UntypedType -> OpaqueType
forcetype :: Any a => OpaqueAst -> Ast a

typecheck_type IntUT = OpaqueType IntTT
typecheck_type (ArrowUT a b) =
  case typecheck_type a of
    OpaqueType at -> case typecheck_type b of
      OpaqueType bt -> OpaqueType $ ArrowTT at bt

typecheck_ast a = typecheck_inner (\_ -> Nothing) a where
  typecheck_inner :: (String -> Maybe UntypedType) -> UntypedAst -> OpaqueAst
  typecheck_inner ctx AddU = OpaqueAst $ AddT
  typecheck_inner ctx MultU = OpaqueAst $ MultT
  typecheck_inner ctx NegateU = OpaqueAst $ NegateT
  typecheck_inner ctx (LiteralU n) = OpaqueAst $ LiteralT n
  typecheck_inner ctx (AppU func a) =
    make_app (typecheck_inner ctx func) (typecheck_inner ctx a)
  typecheck_inner ctx (VarU name) = case ctx name of
    Nothing -> OpaqueAst $ (ErrorT ("undefined variable: " ++ name) :: Ast Void)
    Just ty -> case typecheck_type ty of
      OpaqueType tt -> OpaqueAst $ VarT name tt
  typecheck_inner ctx (LambdaU name ty body) =
    let newctx a = if a == name then Just ty else ctx a in
    case typecheck_inner newctx body of
      OpaqueAst bodynode -> case typecheck_type ty of
        OpaqueType tt -> OpaqueAst $ LambdaT name tt bodynode
  typecheck_inner ctx IdU =
    OpaqueAstPoly (BothTL HereTL HereTL) $
      ((PolyAst . OpaqueAst) :: Any a => Ast (a -> a) -> PolyAst a)
      IdT
  typecheck_inner ctx ComposeU =
    -- should be (BothTL (BothTL HereTL HereTL) (BothTL (BothTL HereTL HereTL) (BothTL HereTL HereTL))) but not used for now
    OpaqueAstPoly (LeftTL $ BothTL HereTL HereTL) $
      ((PolyAst . OpaqueAst) :: Any a => Ast ((a -> a) -> (a -> a) -> (a -> a)) -> PolyAst a)
      ComposeT
  typecheck_inner ctx ApplyU = OpaqueAstPoly (LeftTL HereTL) (helper ApplyT)
    where
      helper :: forall a. Any a => (forall b. Any b => Ast (a -> (a -> b) -> b)) -> PolyAst a
      helper arg =
        let
          inner :: Any a => (forall b. Any b => PolyAst b) -> PolyAst a
          inner polyast = PolyAst $ OpaqueAstPoly (RightTL $ LeftTL $ RightTL HereTL) polyast
        in inner ( (PolyAst . OpaqueAst :: Any b => Ast (a -> (a -> b) -> b) -> PolyAst b) arg)
  typecheck_inner ctx Choose2U =
    OpaqueAstPoly (RightTL $ LeftTL HereTL) $
      ((PolyAst . OpaqueAst) :: Any a => Ast (Int -> a -> a -> a) -> PolyAst a)
      Choose2T

forcetype (OpaqueAst a) = case cast a of
  Just x -> x
  Nothing -> ErrorT $ "wrong type of: " ++ show a

eval :: Any a => Store -> Ast a -> a
eval s AddT = (\a -> \b -> a + b)
eval s MultT = (\a -> \b -> a * b)
eval s NegateT = (\a -> -a)
eval s (LiteralT n) = n
eval s (AppT func a) = (eval s func) (eval s a)
eval s (VarT name tt) = let Just val = s name tt in val
eval s (LambdaT name tt body) = result where
  result param = eval new_s body where
    new_s :: forall a. Any a => String -> Type a -> Maybe a
    new_s v t = case (cast param) of
      Just p ->
        if v == name
          then Just p  -- very important 'Just' here, it compiles without that too!
          else s v t
      Nothing -> s v t
eval s IdT = (\x -> x)
eval s ComposeT = (\f -> \g -> \x -> f (g x))
eval s ApplyT = (\x -> \f -> f x)
eval s Choose2T = (\x -> \a -> \b -> if x == 0 then a else b)

example1 =
  AppU (LambdaU "arg" (ArrowUT IntUT (ArrowUT IntUT IntUT)) (AppU (AppU (VarU "arg") (LiteralU 11)) (LiteralU 6))) AddU

example2 = AppU IdU (LiteralU 5)

example3 = AppU
  (AppU (AppU ComposeU
    (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) )
  ) (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) ) ) (LiteralU 3)

example4 =
  ApplyU `AppU` (LambdaU "x" IntUT $ MultU `AppU` VarU "x" `AppU` VarU "x") `AppU`
  (LambdaU "f" (ArrowUT IntUT IntUT) $ VarU "f" `AppU` LiteralU 7)

example5 = (Choose2U `AppU` (LiteralU 0) `AppU` AddU `AppU` MultU) `AppU` (LiteralU 1) `AppU` (LiteralU 2)

main = do
  putStrLn $ show $ (typecheck_ast example4)
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example1) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example2) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example3) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example4) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example5) :: Ast Int )
