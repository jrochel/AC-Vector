{- |
  4-dimensional linear transformations.
-}

module Data.Vector.Transform.T4 where

import Data.Monoid

import Data.Vector.Class
import Data.Vector.V4

{- |
  The type of 4D linear transformations.

  Note the @Monoid@ instance, which gives you access to the identity transform (@mempty@) and the ability to combine a series of transforms into a single transform (@mappend@).
-}
data Transform4 =
    Transform4
    {
      t4_XX, t4_YX, t4_ZX, t4_WX, t4_1X,
      t4_XY, t4_YY, t4_ZY, t4_WY, t4_1Y,
      t4_XZ, t4_YZ, t4_ZZ, t4_WZ, t4_1Z,
      t4_XW, t4_YW, t4_ZW, t4_WW, t4_1W :: {-# UNPACK #-} !Scalar
    }
  deriving (Eq, Show)

instance Monoid Transform4 where
  mempty = Transform4  1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 1 0
  mappend a b =
    Transform4
    {
      t4_XX = t4_XX a * t4_XX b  +  t4_XY a * t4_YX b  +  t4_XZ a * t4_ZX b  +  t4_XW a * t4_WX b,
      t4_YX = t4_YX a * t4_XX b  +  t4_YY a * t4_YX b  +  t4_YZ a * t4_ZX b  +  t4_YW a * t4_WX b,
      t4_ZX = t4_ZX a * t4_XX b  +  t4_ZY a * t4_YX b  +  t4_ZZ a * t4_ZX b  +  t4_ZW a * t4_WX b,
      t4_WX = t4_WX a * t4_XX b  +  t4_WY a * t4_YX b  +  t4_WZ a * t4_ZX b  +  t4_WW a * t4_WX b,
      t4_1X = t4_1X a * t4_XX b  +  t4_1Y a * t4_YX b  +  t4_1Z a * t4_ZX b  +  t4_1W a * t4_WX b  +  t4_1X b,

      t4_XY = t4_XX a * t4_XY b  +  t4_XY a * t4_YY b  +  t4_XZ a * t4_ZY b  +  t4_XW a * t4_WY b,
      t4_YY = t4_YX a * t4_XY b  +  t4_YY a * t4_YY b  +  t4_YZ a * t4_ZY b  +  t4_YW a * t4_WY b,
      t4_ZY = t4_ZX a * t4_XY b  +  t4_ZY a * t4_YY b  +  t4_ZZ a * t4_ZY b  +  t4_ZW a * t4_WY b,
      t4_WY = t4_WX a * t4_XY b  +  t4_WY a * t4_YY b  +  t4_WZ a * t4_ZY b  +  t4_WW a * t4_WY b,
      t4_1Y = t4_1X a * t4_XY b  +  t4_1Y a * t4_YY b  +  t4_1Z a * t4_ZY b  +  t4_1W a * t4_WY b  +  t4_1Y b,

      t4_XZ = t4_XX a * t4_XZ b  +  t4_XY a * t4_YZ b  +  t4_XZ a * t4_ZZ b  +  t4_XW a * t4_WZ b,
      t4_YZ = t4_YX a * t4_XZ b  +  t4_YY a * t4_YZ b  +  t4_YZ a * t4_ZZ b  +  t4_YW a * t4_WZ b,
      t4_ZZ = t4_ZX a * t4_XZ b  +  t4_ZY a * t4_YZ b  +  t4_ZZ a * t4_ZZ b  +  t4_ZW a * t4_WZ b,
      t4_WZ = t4_WX a * t4_XZ b  +  t4_WY a * t4_YZ b  +  t4_WZ a * t4_ZZ b  +  t4_WW a * t4_WZ b,
      t4_1Z = t4_1X a * t4_XZ b  +  t4_1Y a * t4_YZ b  +  t4_1Z a * t4_ZZ b  +  t4_1W a * t4_WZ b  +  t4_1Z b,

      t4_XW = t4_XX a * t4_XW b  +  t4_XY a * t4_YW b  +  t4_XZ a * t4_ZW b  +  t4_XW a * t4_WW b,
      t4_YW = t4_YX a * t4_XW b  +  t4_YY a * t4_YW b  +  t4_YZ a * t4_ZW b  +  t4_YW a * t4_WW b,
      t4_ZW = t4_ZX a * t4_XW b  +  t4_ZY a * t4_YW b  +  t4_ZZ a * t4_ZW b  +  t4_ZW a * t4_WW b,
      t4_WW = t4_WX a * t4_XW b  +  t4_WY a * t4_YW b  +  t4_WZ a * t4_ZW b  +  t4_WW a * t4_WW b,
      t4_1W = t4_1X a * t4_XW b  +  t4_1Y a * t4_YW b  +  t4_1Z a * t4_ZW b  +  t4_1W a * t4_WW b  +  t4_1W b
    }

-- | Apply a 4D transformation to a 4D point, yielding a new 4D point.
transformP4 :: Transform4 -> Vector4 -> Vector4
transformP4 a (Vector4 x y z w) =
  Vector4
  {
    v4x = t4_XX a * x + t4_YX a * y + t4_ZX a * z + t4_WX a * w + t4_1X a,
    v4y = t4_XY a * x + t4_YY a * y + t4_ZY a * z + t4_WY a * w + t4_1Y a,
    v4z = t4_XZ a * x + t4_YZ a * y + t4_ZZ a * z + t4_WZ a * w + t4_1Z a,
    v4w = t4_XW a * x + t4_YW a * y + t4_ZW a * z + t4_WW a * w + t4_1W a
  }
