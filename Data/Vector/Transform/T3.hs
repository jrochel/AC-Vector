{- |
  3-dimensional linear transformations.
-}

module Data.Vector.Transform.T3 where

import Data.Monoid

import Data.Vector.Class
import Data.Vector.V3

{- |
  The type of 3D linear transformations.

  Note the @Monoid@ instance, which gives you access to the identity transform (@mempty@) and the ability to combine a series of transforms into a single transform (@mappend@).
-}
data Transform3 =
    Transform3
    {
      t3_XX, t3_YX, t3_ZX, t3_1X,
      t3_XY, t3_YY, t3_ZY, t3_1Y,
      t3_XZ, t3_YZ, t3_ZZ, t3_1Z :: {-# UNPACK #-} !Scalar
    }
  deriving (Eq, Show)

instance Monoid Transform3 where
  mempty = Transform3  1 0 0 0  0 1 0 0  0 0 1 0
  mappend a b =
    Transform3
    {
      t3_XX = t3_XX a * t3_XX b  +  t3_XY a * t3_YX b  +  t3_XZ a * t3_ZX b,
      t3_YX = t3_YX a * t3_XX b  +  t3_YY a * t3_YX b  +  t3_YZ a * t3_ZX b,
      t3_ZX = t3_ZX a * t3_XX b  +  t3_ZY a * t3_YX b  +  t3_ZZ a * t3_ZX b,
      t3_1X = t3_1X a * t3_XX b  +  t3_1Y a * t3_YX b  +  t3_1Z a * t3_ZX b  +  t3_1X b,

      t3_XY = t3_XX a * t3_XY b  +  t3_XY a * t3_YY b  +  t3_XZ a * t3_ZY b,
      t3_YY = t3_YX a * t3_XY b  +  t3_YY a * t3_YY b  +  t3_YZ a * t3_ZY b,
      t3_ZY = t3_ZX a * t3_XY b  +  t3_ZY a * t3_YY b  +  t3_ZZ a * t3_ZY b,
      t3_1Y = t3_1X a * t3_XY b  +  t3_1Y a * t3_YY b  +  t3_1Z a * t3_ZY b  +  t3_1Y b,

      t3_XZ = t3_XX a * t3_XZ b  +  t3_XY a * t3_YZ b  +  t3_XZ a * t3_ZZ b,
      t3_YZ = t3_YX a * t3_XZ b  +  t3_YY a * t3_YZ b  +  t3_YZ a * t3_ZZ b,
      t3_ZZ = t3_ZX a * t3_XZ b  +  t3_ZY a * t3_YZ b  +  t3_ZZ a * t3_ZZ b,
      t3_1Z = t3_1X a * t3_XZ b  +  t3_1Y a * t3_YZ b  +  t3_1Z a * t3_ZZ b  +  t3_1Z b
    }

-- | Apply a 3D transformation to a 3D point, yielding a new 3D point.
transformP3 :: Transform3 -> Vector3 -> Vector3
transformP3 a (Vector3 x y z) =
  Vector3
  {
    v3x = t3_XX a * x + t3_YX a * y + t3_ZX a * z + t3_1X a,
    v3y = t3_XY a * x + t3_YY a * y + t3_ZY a * z + t3_1Y a,
    v3z = t3_XZ a * x + t3_YZ a * y + t3_ZZ a * z + t3_1Z a
  }
