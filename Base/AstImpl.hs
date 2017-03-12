{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.AstImpl where
import Base.Pervasives
import Base.Types
import Base.Ast

deriving instance Typeable1 Store
deriving instance Typeable1 Env
deriving instance Typeable2 Ast
deriving instance Show (Ast e a)

instance Show (Mono (Ast e)) where
  show (Mono a) = show a
