{- |
  2-dimensional linear transformations.
-}

module Data.Vector.Transform.T2 where

import Data.Monoid

import Data.Vector.Class
import Data.Vector.V2

{- |
  The type of 2D linear transformations.

  Note the @Monoid@ instance, which gives you access to the identity transform (@mempty@) and the ability to combine a series of transforms into a single transform (@mappend@).
-}
data Transform2 =
    Transform2
    {
      t2_XX, t2_YX, t2_1X,
      t2_XY, t2_YY, t2_1Y :: {-# UNPACK #-} !Scalar
    }
  deriving (Eq, Show)

instance Monoid Transform2 where
  mempty = Transform2  1 0 0  0 1 0
  mappend a b =
    Transform2
    {
      t2_XX = t2_XX a * t2_XX b  +  t2_XY a * t2_YX b,
      t2_YX = t2_YX a * t2_XX b  +  t2_YY a * t2_YX b,
      t2_1X = t2_1X a * t2_XX b  +  t2_1Y a * t2_YX b  +  t2_1X b,

      t2_XY = t2_XX a * t2_XY b  +  t2_XY a * t2_YY b,
      t2_YY = t2_YX a * t2_XY b  +  t2_YY a * t2_YY b,
      t2_1Y = t2_1X a * t2_XY b  +  t2_1Y a * t2_YY b  +  t2_1Y b
    }

-- | Apply a 2D transformation to a 2D point, yielding a new 2D point.
transformP2 :: Transform2 -> Vector2 -> Vector2
transformP2 a (Vector2 x y) =
  Vector2
  {
    v2x = t2_XX a * x + t2_YX a * y + t2_1X a,
    v2y = t2_XY a * x + t2_YY a * y + t2_1Y a
  }
