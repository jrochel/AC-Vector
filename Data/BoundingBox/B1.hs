{- |
  This module provides the 'BBox1' type (mainly for completeness).
-}

module Data.BoundingBox.B1 where

import Data.Vector.Class
import Data.Vector.V1
import qualified Data.BoundingBox.Range as R

-- | The 'BBox1' type is basically a 'Range', but all the operations over it work with 'Vector1' (which is really 'Scalar'). While it's called a bounding /box/, a 1-dimensional box is in truth a simple line interval, just like 'Range'.
newtype BBox1 = BBox1 {range :: R.Range} deriving (Eq, Show)

-- | Given two vectors, construct a bounding box (swapping the endpoints if necessary).
bound_corners :: Vector1 -> Vector1 -> BBox1
bound_corners (Vector1 xa) (Vector1 xb) = BBox1 $ R.bound_corners xa xb

-- | Find the bounds of a list of points. (Throws an exception if the list is empty.)
bound_points :: [Vector1] -> BBox1
bound_points = BBox1 . R.bound_points . map v1x

-- | Test whether a 'Vector1' lies within a 'BBox1'.
within_bounds :: Vector1 -> BBox1 -> Bool
within_bounds (Vector1 x) (BBox1 r) = x `R.within_bounds` r

-- | Return the minimum endpoint for a 'BBox1'.
min_point :: BBox1 -> Vector1
min_point = Vector1 . R.min_point . range

-- | Return the maximum endpoint for a 'BBox1'.
max_point :: BBox1 -> Vector1
max_point = Vector1 . R.max_point . range

-- | Take the union of two 'BBox1' values. The result is a new 'BBox1' that contains all the points the original boxes contained, plus any extra space between them.
union :: BBox1 -> BBox1 -> BBox1
union (BBox1 r0) (BBox1 r1) = BBox1 (r0 `R.union` r1)

-- | Take the intersection of two 'BBox1' values. If the boxes do not overlap, return 'Nothing'. Otherwise return a 'BBox1' containing only the points common to both argument boxes.
isect :: BBox1 -> BBox1 -> Maybe BBox1
isect (BBox1 r0) (BBox1 r1) = do
  r <- (r0 `R.isect` r1)
  return (BBox1 r)

-- | Efficiently compute the union of a list of bounding boxes.
unions :: [BBox1] -> BBox1
unions = BBox1 . R.unions . map range
