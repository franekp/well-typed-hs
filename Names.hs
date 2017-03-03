{-# LANGUAGE EmptyDataDecls,
 MultiParamTypeClasses,
 GADTs,
 RankNTypes,
 StandaloneDeriving,
 DeriveDataTypeable,
 ScopedTypeVariables,
 FunctionalDependencies,
 OverlappingInstances,
 FlexibleInstances,
 FlexibleContexts,
 ExistentialQuantification,
 UndecidableInstances,
 TypeFamilies #-}

module Names where

import qualified Data.Char

import BaseTypes

instance Any NameZero where
  get_type = TypeVarTT $ ZeroTV

instance Name a => Any (NameSucc a) where
  get_type = TypeVarTT $ SuccTV get_type_variable

instance Name NameZero where
  type_variable_name ZeroTV = "a"
  get_type_variable = ZeroTV

instance Name a => Name (NameSucc a) where
  type_variable_name (SuccTV a) = case type_variable_name a of
    'z':t -> 'a':'\'':t
    c:t -> (:t) $ Data.Char.chr $ (+1) $ Data.Char.ord c
  get_type_variable = SuccTV get_type_variable
