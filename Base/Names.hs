{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Names where
import Base.Pervasives
import Base.Letters (Letter)

data Name :: * -> * where
  NilN ::
    Name Nil
  ConsN :: (A Letter a, A Name name) =>
    Letter a -> Name name -> Name (Cons a name)
