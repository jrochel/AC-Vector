{- |
  3-dimensional vectors with vector arithmetic.
-}

module Data.Vector.V3 where

import Data.Vector.Class

data Vector3 = Vector3 {v3x, v3y, v3z :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

instance BasicVector Vector3 where
  vmap  f (Vector3 x  y  z )                    = Vector3 (f x)     (f y)     (f z)
  vzip  f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (f x1 x2) (f y1 y2) (f z1 z2)
  vfold f (Vector3 x  y  z )                    = f x (f y z)

  vpack (x:y:z:_) = Just $ Vector3 x y z
  vpack _         = Nothing

  vunpack (Vector3 x y z) = [x,y,z]

  vpromote x = Vector3 x x x

instance Num Vector3 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  abs = vmap abs
  signum = vmap signum
  fromInteger = vpromote . fromInteger

instance Fractional Vector3 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational = vpromote . fromRational

instance Vector Vector3 where

{- |
  Take the /cross product/ of two 3D vectors. This produces a new 3D vector that is perpendicular to the plane of the first two vectors, and who's length is equal to the sine of the angle between those vectors multiplied by their lengths.

  Note that @a \`vcross\` b = negate (b \`vcross\` a)@.
-}
vcross :: Vector3 -> Vector3 -> Vector3
vcross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
  Vector3
  {
    v3x = y1 * z2   -   y2 * z1,
    v3y = z1 * x2   -   z2 * x1,
    v3z = x1 * y2   -   x2 * y1
  }
