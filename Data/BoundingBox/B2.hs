{- |
  This module provides the 'BBox2' type for 2-dimensional bounding boxes.
-}

module Data.BoundingBox.B2 where

import Data.Vector.Class
import Data.Vector.V2
import qualified Data.BoundingBox.Range as R

-- | A 'BBox2' is a 2D bounding box (aligned to the coordinate axies).
data BBox2 = BBox2 {minX, minY, maxX, maxY :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

-- | Return the X-range that this bounding box covers.
rangeX :: BBox2 -> R.Range
rangeX b = R.Range (minX b) (maxX b)

-- | Return the Y-range that this bounding box covers.
rangeY :: BBox2 -> R.Range
rangeY b = R.Range (minY b) (maxY b)

-- | Given ranges for each coordinate axis, construct a bounding box.
rangeXY :: R.Range -> R.Range -> BBox2
rangeXY (R.Range x0 x1) (R.Range y0 y1) = BBox2 x0 y0 x1 y1

-- | Given a pair of corner points, construct a bounding box. (The points must be from opposite corners, but it doesn't matter /which/ corners nor which order they are given in.)
bound_corners :: Vector2 -> Vector2 -> BBox2
bound_corners (Vector2 xa ya) (Vector2 xb yb) = BBox2 (min xa xb) (min ya yb) (max xa xb) (max ya yb)

-- | Find the bounds of a list of points. (Throws an exception if the list is empty.)
bound_points :: [Vector2] -> BBox2
bound_points ps =
  let
    xs = map v2x ps
    ys = map v2y ps
  in BBox2 (minimum xs) (minimum ys) (maximum xs) (maximum ys)

-- | Test whether a given 2D vector is inside this bounding box.
within_bounds :: Vector2 -> BBox2 -> Bool
within_bounds (Vector2 x y) b =
  x `R.within_bounds` (rangeX b) &&
  y `R.within_bounds` (rangeY b)

-- | Return the minimum values for both coordinates. (In usual 2D space, the bottom-left corner point.)
min_point :: BBox2 -> Vector2
min_point (BBox2 x0 y0 x1 y1) = Vector2 x0 y0

-- | Return the maximum values for both coordinates. (In usual 2D space, the top-right corner point.)
max_point :: BBox2 -> Vector2
max_point (BBox2 x0 y0 x1 y1) = Vector2 x1 y1

-- | Take the union of two bounding boxes. The result is a new bounding box that contains all the points the original boxes contained, plus any extra space between them.
union :: BBox2 -> BBox2 -> BBox2
union b0 b1 =
  let
    rx = (rangeX b0) `R.union` (rangeX b1)
    ry = (rangeY b0) `R.union` (rangeY b1)
  in rangeXY rx ry

-- | Take the intersection of two bounding boxes. If the boxes do not overlap, return 'Nothing'. Otherwise return a new bounding box containing only the points common to both argument boxes.
isect :: BBox2 -> BBox2 -> Maybe BBox2
isect b0 b1 = do
  rx <- (rangeX b0) `R.isect` (rangeX b1)
  ry <- (rangeY b0) `R.isect` (rangeY b1)
  return (rangeXY rx ry)

-- | Efficiently compute the union of a list of bounding boxes.
unions :: [BBox2] -> BBox2
unions bs =
  let
    minP = map min_point bs
    maxP = map max_point bs
  in
    BBox2
      (minimum $ map v2x minP) (minimum $ map v2y minP)
      (maximum $ map v2x maxP) (maximum $ map v2y maxP)
