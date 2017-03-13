{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.ChrSym where

-- NOTE: this is a kind
data Chr = A_ | B_ | C_ | D_ | E_ | F_ | G_ | H_ | I_ | J_ | K_ | L_ | M_
  | N_ | O_ | P_ | Q_ | R_ | S_ | T_ | U_ | V_ | W_ | X_ | Y_ | Z_ | CapA_
  | CapB_ | CapC_ | CapD_ | CapE_ | CapF_ | CapG_ | CapH_ | CapI_ | CapJ_
  | CapK_ | CapL_ | CapM_ | CapN_ | CapO_ | CapP_ | CapQ_ | CapR_ | CapS_
  | CapT_ | CapU_ | CapV_ | CapW_ | CapX_ | CapY_ | CapZ_

-- this #define in settings.hs is a workaround because GHC doesn't yet support
-- kind synonyms; the declaration below is only for readability
#undef Sym

-- NOTE: this is a kind synonym
type Sym = [Chr]
