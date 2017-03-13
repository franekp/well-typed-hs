{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Symbol where
import Base.Pervasives
import Base.ChrRep

infixr `ConsSYM`

data Symbol :: Sym -> * where
  NilSYM ::
    Symbol '[]
  ConsSYM :: (A ChrRep a, A Symbol s) =>
    ChrRep a -> Symbol s -> Symbol (a ': s)

type instance T Symbol = Symbol
