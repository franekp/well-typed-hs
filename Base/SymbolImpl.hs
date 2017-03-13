{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.SymbolImpl where
import Base.Pervasives
import Base.Symbol
import Base.ChrRep
import Base.ChrRepImpl

instance Show (Symbol a) where
  show NilSYM = ""
  show (ConsSYM h t) = show h ++ show t

instance Show (Mono Symbol) where
  show (Mono a) = show a

instance Read (Mono Symbol) where
  readsPrec _ "" = [(Mono NilSYM, "")]
  readsPrec prec (h:t) = case read [h] :: Mono ChrRep of
    Mono h' -> do
      (ww, rest) <- (readsPrec prec :: ReadS (Mono Symbol)) t
      case ww of
        Mono w -> return (Mono (ConsSYM h' w), rest)

instance A Symbol '[] where
  anything = NilSYM

instance (A ChrRep a, A Symbol s) => A Symbol (a ': s) where
  anything = ConsSYM anything anything

deriving instance Eq (Symbol a)
deriving instance Typeable Symbol

instance Eq (Mono Symbol) where
  Mono a == Mono b = case cast b of
    Just bb -> a == bb
    Nothing -> False
