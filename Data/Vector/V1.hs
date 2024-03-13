{- |
  1-dimensional vectors with vector arithmetic.

  This isn't especially useful. Usually if you want to calculate with scalars, you can just use the 'Scalar' type directly. However, this module provides a 'Vector1' newtype over 'Scalar' that allows a scalar to be treated as a sort of vector, which is very occasionally useful.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Vector.V1 where

import Data.Vector.Class

{- |
  The type of 1D vectors.

  Owing to its particularly simple structure, this type has more class instances than \'propper\' vectors have. Still, for the most part you'll probably want to just use 'Scalar' itself directly.
-}
newtype Vector1 = Vector1 {v1x :: Scalar} deriving (Eq, Ord, Enum, Show, Num, Fractional)

instance BasicVector Vector1 where
  vmap  f (Vector1 x )              = Vector1 (f x)
  vzip  f (Vector1 x1) (Vector1 x2) = Vector1 (f x1 x2)
  vfold _ (Vector1 x )              = x

  vpack (x:_) = Just $ Vector1 x
  vpack _     = Nothing

  vunpack (Vector1 x) = [x]

  vpromote x = Vector1 x

instance Vector Vector1 where
