{- |
  4-dimensional vectors with vector arithmetic.
-}

module Data.Vector.V4 where

import Data.Vector.Class

data Vector4 = Vector4 {v4x, v4y, v4z, v4w :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

instance BasicVector Vector4 where
  vmap  f (Vector4 x  y  z  w )                       = Vector4 (f x)     (f y)     (f z)     (f w)
  vzip  f (Vector4 x1 y1 z1 w1) (Vector4 x2 y2 z2 w2) = Vector4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
  vfold f (Vector4 x  y  z  w )                       = f (f x y) (f z w)

  vpack (x:y:z:w:_) = Just $ Vector4 x y z w
  vpack _           = Nothing

  vunpack (Vector4 x y z w) = [x,y,z,w]

  vpromote x = Vector4 x x x x

instance Num Vector4 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  abs = vmap abs
  signum = vmap signum
  fromInteger = vpromote . fromInteger

instance Fractional Vector4 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational = vpromote . fromRational

instance Vector Vector4 where
