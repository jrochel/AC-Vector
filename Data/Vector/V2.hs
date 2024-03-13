{- |
  2-dimensional vectors with vector arithmetic.
-}

module Data.Vector.V2 where

import Data.Vector.Class

data Vector2 = Vector2 {v2x, v2y :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

instance BasicVector Vector2 where
  vmap  f (Vector2 x  y )                 = Vector2 (f x)     (f y)
  vzip  f (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (f x1 x2) (f y1 y2)
  vfold f (Vector2 x  y )                 = f x y

  vpack (x:y:_) = Just $ Vector2 x y
  vpack _       = Nothing

  vunpack (Vector2 x y) = [x,y]

  vpromote x = Vector2 x x

instance Num Vector2 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  abs = vmap abs
  signum = vmap signum
  fromInteger = vpromote . fromInteger

instance Fractional Vector2 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational = vpromote . fromRational

instance Vector Vector2 where
