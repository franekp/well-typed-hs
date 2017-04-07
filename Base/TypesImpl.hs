{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.TypesImpl where
import Base.Pervasives
import Base.Types
import Base.Symbol
import Base.SymbolImpl
import qualified Data.Char

show_value :: forall a. A Type a => a -> String
show_value a = case (anything :: Type a) of
  _ :-> _ -> "<func>"
  IntT -> show (a :: Int)
  VoidT -> "<void>"
  TypeVarT v -> "(undefined :: " ++ show v ++ ")"
  RecordT r -> show a
  HasFieldT _ _ -> error "this code should be unreachable"

instance A TypeVar Zero where
  anything = ZeroTV

instance A TypeVar a => A TypeVar (Succ a) where
  anything = SuccTV anything

instance (A Type a, A Type b) => A Type (a -> b) where
  anything = anything :-> anything

instance A Type Int where
  anything = IntT

instance A Type Void where
  anything = VoidT

instance A TypeVar a => A Type (RuntimeTypeVar a) where
  anything = TypeVarT anything

instance A RecordType a => A Type (Record a) where
  anything = RecordT anything

instance (A Type rest, A Type a, A FieldName f) => A Type (HasField '(f, a) rest) where
  anything = HasFieldT (anything, anything) anything

deriving instance Eq (Type a)
instance Eq (Mono Type) where
  -- first all uninhabitable types that are heavily used as dummy variables
  -- for the purpose of unification of parameters of run-time type-level
  -- functions; for some complex reasons they all need to be considered equal
  Mono VoidT == _ = True
  _ == Mono VoidT = True
  Mono (TypeVarT _) == _ = True
  _ == Mono (TypeVarT _) = True
  Mono (a :-> b) == Mono (a' :-> b') = Mono a == Mono a' && Mono b == Mono b'
  Mono IntT == Mono IntT = True
  Mono (RecordT a) == Mono (RecordT a') = Mono a == Mono a'
  Mono (HasFieldT (f, a) rest) == Mono rest' = Mono rest == Mono rest'
  Mono rest == Mono (HasFieldT (f', a') rest') = Mono rest == Mono rest'
  _ == _ = False

deriving instance Eq (TypeVar a)
deriving instance Typeable TypeVar
instance Eq (Mono TypeVar) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False

instance Ord (Mono TypeVar) where
  Mono ZeroTV <= Mono _ = True
  Mono (SuccTV a) <= Mono ZeroTV = False
  Mono (SuccTV a) <= Mono (SuccTV b) = Mono a <= Mono b

instance Show (Type a) where
  show ((a :-> b) :-> c) = "(" ++ show (a :-> b) ++ ") -> " ++ show c
  show (a :-> b) = show a ++ " -> " ++ show b
  show IntT = "Int"
  show VoidT = "Void"
  show (TypeVarT a) = show a
  show (RecordT r) = show r
  show (HasFieldT (f, a) r) = "HasField(" ++ show f ++ ":" ++ show a ++ ") " ++ show r

instance Show (Mono Type) where
  show (Mono a) = show a

instance Show (TypeVar a) where
  show ZeroTV = "a"
  show (SuccTV a) = case show a of
    'z':t -> 'a':'\'':t
    c:t -> (:t) $ Data.Char.chr $ (+1) $ Data.Char.ord c
    [] -> error "unreachable"

instance Show (Mono TypeVar) where
  show (Mono a) = show a

instance (Show (Mono t), T t ~ Type) => Show (Poly t) where
  show qq = "forall" ++ str ZeroTV qq where
    str :: (A TypeVar a, Show (Mono t)) => TypeVar a -> Poly t -> String
    str last_tv (MonoP tp) = ". " ++ show tp
    str last_tv (ForallP ident poly) = " " ++ show last_tv ++ show ident ++ do_stuff last_tv poly where
      do_stuff :: (A TypeVar a, Show (Mono t)) => TypeVar a -> ExistsPoly t (RuntimeTypeVar a) -> String
      do_stuff tv (ExistsPoly x) = str (SuccTV tv) x

instance A Symbol f => A FieldName f where
  anything = FieldName anything

deriving instance Eq (FieldName f)
deriving instance Typeable FieldName

instance Eq (Mono FieldName) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False

instance Show (FieldName f) where
  show (FieldName x) = show x

instance Show (Mono FieldName) where
  show (Mono a) = show a

instance Read (Mono FieldName) where
  readsPrec prec input = map f $ (readsPrec prec :: ReadS (Mono Symbol)) input where
    f (mono_a, b) = case mono_a of
      Mono a -> (Mono $ FieldName a, b)

instance A RecordType '[] where
  anything = NilRT

instance (A FieldName f, A Type a, A RecordType r) => A RecordType ('(f, a) ': r) where
  anything = (anything, anything) `ConsRT` anything

deriving instance Eq (RecordType r)
deriving instance Typeable RecordType

instance Eq (Mono RecordType) where
  Mono NilRT == Mono NilRT = True
  Mono (ConsRT (f, a) rest) == Mono (ConsRT (f', a') rest') =
    Mono f == Mono f' && Mono a == Mono a' && Mono rest == Mono rest'
  _ == _ = False

instance Show (RecordType r) where
  show = ("{" ++) . (++ "}") . inner where
    inner :: RecordType a -> String
    inner NilRT = "::"
    inner ((f, t) `ConsRT` NilRT) = show f ++ " :: " ++ show t
    inner ((f, t) `ConsRT` rest) = show f ++ " :: " ++ show t ++ ", " ++ inner rest

instance Show (Mono RecordType) where
  show (Mono a) = show a

instance Show (Record r) where
  show = ("{" ++) . (++ "}") . inner where
    inner :: Record a -> String
    inner NilRC = ""
    inner ((f, t) `ConsRC` NilRC) = show f ++ " = " ++ show_value t
    inner ((f, t) `ConsRC` rest) = show f ++ " = " ++ show_value t ++ ", " ++ inner rest

instance Show (Mono Record) where
  show (Mono a) = show a
