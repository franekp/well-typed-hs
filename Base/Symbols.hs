{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Symbols where
import Base.Pervasives
import Base.Letters (Letter)

infixr `ConsSYM`

data Symbol :: [*] -> * where
  NilSYM ::
    Symbol '[]
  ConsSYM :: (A Letter a, A Symbol s) =>
    Letter a -> Symbol s -> Symbol (a ': s)

type instance T Symbol = Symbol
