{- |
  1-dimensional linear transformations.
-}

module Data.Vector.Transform.T1 where

import Data.Monoid

import Data.Vector.Class
import Data.Vector.V1

{- |
  The type of 1D linear transformations. Essentially, this is applying a linear function to a number.

  Note the @Monoid@ instance, which gives you access to the identity transform (@mempty@) and the ability to combine a series of transforms into a single transform (@mappend@).
-}
data Transform1 =
    Transform1
    {
      t1_XX, t1_1X :: {-# UNPACK #-} !Scalar
    }
  deriving (Eq, Show)

instance Monoid Transform1 where
  mempty = Transform1  1 0
  mappend a b =
    Transform1
    {
      t1_XX = t1_XX a * t1_XX b,
      t1_1X = t1_1X a * t1_XX b  +  t1_1X b
    }

-- | Apply a 1D transformation to a 1D point, yielding a new 1D point.
transformP1 :: Transform1 -> Vector1 -> Vector1
transformP1 a (Vector1 x) = Vector1 (t1_XX a * x + t1_1X a)
