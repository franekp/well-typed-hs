{-# LANGUAGE GADTs, KindSignatures #-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fwarn-incomplete-uni-patterns
#-}

newtype Wrapper = Wrapper String

data Type :: * -> * where
  IntT :: Type Int
  WrapperT :: Type Wrapper

should_be_exhaustive :: Type Int -> ()
should_be_exhaustive x = case x of
  IntT -> ()

main = putStrLn ""
