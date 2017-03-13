{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Symbol where
import Base.Pervasives
import Base.ChrRep (Chr, ChrRep)

infixr `ConsSYM`

#undef Sym
-- this #define in settings.hs is a workaround because GHC doesn't yet support
-- kind synonyms; the declaration below is only for readability
type Sym = [Chr]

data Symbol :: [Chr] -> * where
  NilSYM ::
    Symbol '[]
  ConsSYM :: (A ChrRep a, A Symbol s) =>
    ChrRep a -> Symbol s -> Symbol (a ': s)

type instance T Symbol = Symbol
