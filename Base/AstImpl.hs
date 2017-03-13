{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.AstImpl where
import Base.Pervasives
import Base.Types
import Base.TypesImpl
import Base.Ast

deriving instance Typeable Store
deriving instance Typeable Env
deriving instance Typeable Ast
deriving instance Show (Ast e a)

instance Show (Mono (Ast e)) where
  show (Mono a) = show a
