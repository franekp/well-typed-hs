{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-} {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns -Werror #-}

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
