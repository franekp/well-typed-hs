{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.TypesImpl where
import Base.Pervasives
import Base.Types
import Base.Symbol
import Base.SymbolImpl
import qualified Data.Char

show_value :: forall a. A Type a => a -> String
show_value val = case (anything :: Type a) of
  _ :-> _ -> "<func>"
  IntT -> show (val :: Int)
  VoidT -> "<void>"
  TypeVarT a -> "(undefined :: " ++ show a ++ ")"
  RecordT _ -> show val
  HasFieldT _ _ -> error "this code should be unreachable"
  BoolT -> show (val :: Bool)
  MaybeT (t :: Type t) -> case (val :: Maybe t) of
    Nothing -> "Nothing"
    Just a -> "Just (" ++ show_value a ++ ")"
  EitherT (l :: Type l) (r :: Type r) -> case (val :: Either l r) of
    Left a -> "Left (" ++ show_value a ++ ")"
    Right a -> "Right (" ++ show_value a ++ ")"
  CharT -> show (val :: Char)
  ListT CharT -> val :: String
  ListT (t :: Type t) -> case (val :: [t]) of
    [] -> "[]"
    h:t -> "[" ++ show_value h ++ foldl (++) "" (map (("," ++) . show_value) t) ++ "]"
  IO_T t -> "<IO (" ++ show t ++ ")>"
  DynamicT -> show (val :: Dynamic)
  UnitT -> "()"
  PairT _ _ -> let (a, b) = val in "(" ++ show_value a ++ ", " ++ show_value b ++ ")"
  TripleT _ _ _ -> let (a, b, c) = val in "(" ++ show_value a ++ ", " ++ show_value b ++ ", " ++ show_value c ++ ")"

eq_value :: forall a. A Type a => a -> a -> Bool
eq_value v1 v2 = case (anything :: Type a) of
  _ :-> _ -> error "functions are not comparable"
  IntT -> v1 == v2
  VoidT -> error "unreachable"
  TypeVarT a -> error "unreachable"
  RecordT _ -> error "records are not comparable"
  HasFieldT _ _ -> error "unreachable"
  BoolT -> v1 == v2
  MaybeT (t :: Type t) -> case (v1, v2) of
    (Nothing, Nothing) -> True
    (Just a, Just b) -> a `eq_value` b
    _ -> False
  EitherT (l :: Type l) (r :: Type r) -> case (v1, v2) of
    (Left a, Left b) -> a `eq_value` b
    (Right a, Right b) -> a `eq_value` b
    _ -> False
  CharT -> v1 == v2
  ListT (t :: Type t) -> case (v1, v2) of
    ([], []) -> True
    (h1:t1, h2:t2) -> h1 `eq_value` h2 && t1 `eq_value` t2
    _ -> False
  IO_T t -> error "IO values are not comparable"
  DynamicT -> error "Dynamic values are not comparable"
  UnitT -> True
  PairT _ _ -> let
      (a, b) = v1
      (a', b') = v2
    in a `eq_value` a' && b `eq_value` b'
  TripleT _ _ _ -> let
      (a, b, c) = v1
      (a', b', c') = v2
    in a `eq_value` a' && b `eq_value` b' && c `eq_value` c'

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

instance A Type Bool where
  anything = BoolT

instance A Type t => A Type (Maybe t) where
  anything = MaybeT anything

instance (A Type l, A Type r) => A Type (Either l r) where
  anything = EitherT anything anything

instance A Type Char where
  anything = CharT

instance A Type t => A Type [t] where
  anything = ListT anything

instance A Type t => A Type (IO t) where
  anything = IO_T anything

instance A Type Dynamic where
  anything = DynamicT

instance A Type () where
  anything = UnitT

instance (A Type a, A Type b) => A Type (a, b) where
  anything = PairT anything anything

instance (A Type a, A Type b, A Type c) => A Type (a, b, c) where
  anything = TripleT anything anything anything

deriving instance Eq (Type a)
instance Eq (Mono Type) where
  -- first all uninhabitable types that are heavily used as dummy variables
  -- for the purpose of unification of parameters of run-time type-level
  -- functions; for some complex reasons they all need to be considered equal
  Mono VoidT == _ = True
  _ == Mono VoidT = True
  Mono (TypeVarT _) == _ = True
  _ == Mono (TypeVarT _) = True
  -- then record field stuff that is ignored
  Mono (HasFieldT (f, a) rest) == Mono rest' = Mono rest == Mono rest'
  Mono rest == Mono (HasFieldT (f', a') rest') = Mono rest == Mono rest'
  -- then all normal types
  Mono (a :-> b) == arg = case arg of
    Mono (a' :-> b') -> Mono a == Mono a' && Mono b == Mono b'
    _ -> False
  Mono IntT == arg = case arg of
    Mono IntT -> True
    _ -> False
  Mono (RecordT a) == arg = case arg of
    Mono (RecordT a') -> Mono a == Mono a'
    _ -> False
  Mono BoolT == arg = case arg of
    Mono BoolT -> True
    _ -> False
  Mono (MaybeT t) == arg = case arg of
    Mono (MaybeT t') -> Mono t == Mono t'
    _ -> False
  Mono (EitherT l r) == arg = case arg of
    Mono (EitherT l' r') -> Mono l == Mono l' && Mono r == Mono r'
    _ -> False
  Mono CharT == arg = case arg of
    Mono CharT -> True
    _ -> False
  Mono (ListT t) == arg = case arg of
    Mono (ListT t') -> Mono t == Mono t'
    _ -> False
  Mono (IO_T t) == arg = case arg of
    Mono (IO_T t') -> Mono t == Mono t'
    _ -> False
  Mono DynamicT == arg = case arg of
    Mono DynamicT -> True
    _ -> False
  Mono UnitT == arg = case arg of
    Mono UnitT -> True
    _ -> False
  Mono (PairT a b) == arg = case arg of
    Mono (PairT a' b') -> Mono a == Mono a' && Mono b == Mono b'
    _ -> False
  Mono (TripleT a b c) == arg = case arg of
    Mono (TripleT a' b' c') -> Mono a == Mono a' && Mono b == Mono b' && Mono c == Mono c'
    _ -> False

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
  show (HasFieldT (f, a) r) = helper (HasFieldT (f, a) r) [] where
    bla = "HasField(" ++ show f ++ ":" ++ show a ++ ") " ++ show r
    helper :: forall a. Type a -> [String] -> String
    concat_annotations :: [String] -> String
    concat_annotations [] = error "unreachable code"
    concat_annotations [a] = a
    concat_annotations (h:t) = h ++ ", " ++ concat_annotations t
    helper (HasFieldT (f, a) r) li = helper r $ annotation:li where
      annotation = show f ++ " : " ++ show a
    helper t li = "{" ++ show t ++ " | " ++ concat_annotations li ++ "}"

  show BoolT = "Bool"
  show (MaybeT t) = "Maybe (" ++ show t ++ ")"
  show (EitherT l r) = "Either (" ++ show l ++ ") (" ++ show r ++ ")"
  show CharT = "Char"
  show (ListT t) = "[" ++ show t ++ "]"
  show (IO_T t) = "IO (" ++ show t ++ ")"
  show DynamicT = "Dynamic"
  show UnitT = "()"
  show (PairT a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (TripleT a b c) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

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
    inner NilRT = ""
    inner ((f, t) `ConsRT` NilRT) = show f ++ " : " ++ show t
    inner ((f, t) `ConsRT` rest) = show f ++ " : " ++ show t ++ ", " ++ inner rest

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
