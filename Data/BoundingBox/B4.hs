{- |
  This module provides the 'BBox4' type for 4-dimensional bounding boxes (bounding hyper-volumes\).
-}

module Data.BoundingBox.B4 where

import Data.Vector.Class
import Data.Vector.V4
import qualified Data.BoundingBox.Range as R

-- | A 'BBox4' is a 4D bounding box (aligned to the coordinate axies).
data BBox4 = BBox4 {minX, minY, minZ, minW, maxX, maxY, maxZ, maxW :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

-- | Return the X-range that this bounding box covers.
rangeX :: BBox4 -> R.Range
rangeX b = R.Range (minX b) (maxX b)

-- | Return the Y-range that this bounding box covers.
rangeY :: BBox4 -> R.Range
rangeY b = R.Range (minY b) (maxY b)

-- | Return the Z-range that this bounding box covers.
rangeZ :: BBox4 -> R.Range
rangeZ b = R.Range (minZ b) (maxZ b)

-- | Return the W-range (4th coordinate) that this bounding box covers.
rangeW :: BBox4 -> R.Range
rangeW b = R.Range (minW b) (maxW b)

-- | Given ranges for each coordinate axis, construct a bounding box.
rangeXYZW :: R.Range -> R.Range -> R.Range -> R.Range -> BBox4
rangeXYZW (R.Range x0 x1) (R.Range y0 y1) (R.Range z0 z1) (R.Range w0 w1) = BBox4 x0 y0 z0 w0 x1 y1 z1 w1

-- | Given a pair of corner points, construct a bounding box. (The points must be from opposite corners, but it doesn't matter /which/ corners nor which order they are given in.)
bound_corners :: Vector4 -> Vector4 -> BBox4
bound_corners (Vector4 xa ya za wa) (Vector4 xb yb zb wb) =
  BBox4 (min xa xb) (min ya yb) (min za zb) (min wa wb) (max xa xb) (max ya yb) (max za zb) (max wa wb)

-- | Find the bounds of a list of points. (Throws an exception if the list is empty.)
bound_points :: [Vector4] -> BBox4
bound_points ps =
  let
    xs = map v4x ps
    ys = map v4y ps
    zs = map v4z ps
    ws = map v4w ps
  in BBox4 (minimum xs) (minimum ys) (minimum zs) (minimum ws) (maximum xs) (maximum ys) (maximum zs) (maximum ws)

-- | Test whether a given 4D vector is inside this bounding box.
within_bounds :: Vector4 -> BBox4 -> Bool
within_bounds (Vector4 x y z w) b =
  x `R.within_bounds` (rangeX b) &&
  y `R.within_bounds` (rangeY b) &&
  z `R.within_bounds` (rangeZ b) &&
  w `R.within_bounds` (rangeW b)

-- | Return the minimum values for all coordinates.
min_point :: BBox4 -> Vector4
min_point (BBox4 x0 y0 z0 w0 x1 y1 z1 w1) = Vector4 x0 y0 z0 w0

-- | Return the maximum values for all coordinates.
max_point :: BBox4 -> Vector4
max_point (BBox4 x0 y0 z0 w0 x1 y1 z1 w1) = Vector4 x1 y1 z1 w1

-- | Take the union of two bounding boxes. The result is a new bounding box that contains all the points the original boxes contained, plus any extra space between them.
union :: BBox4 -> BBox4 -> BBox4
union b0 b1 =
  let
    rx = (rangeX b0) `R.union` (rangeX b1)
    ry = (rangeY b0) `R.union` (rangeY b1)
    rz = (rangeZ b0) `R.union` (rangeZ b1)
    rw = (rangeW b0) `R.union` (rangeW b1)
  in rangeXYZW rx ry rz rw

-- | Take the intersection of two bounding boxes. If the boxes do not overlap, return 'Nothing'. Otherwise return a new bounding box containing only the points common to both argument boxes.
isect :: BBox4 -> BBox4 -> Maybe BBox4
isect b0 b1 = do
  rx <- (rangeX b0) `R.isect` (rangeX b1)
  ry <- (rangeY b0) `R.isect` (rangeY b1)
  rz <- (rangeZ b0) `R.isect` (rangeZ b1)
  rw <- (rangeW b0) `R.isect` (rangeW b1)
  return (rangeXYZW rx ry rz rw)

-- | Efficiently compute the union of a list of bounding boxes.
unions :: [BBox4] -> BBox4
unions bs =
  let
    minP = map min_point bs
    maxP = map max_point bs
  in
    BBox4
      (minimum $ map v4x minP) (minimum $ map v4y minP) (minimum $ map v4z minP) (minimum $ map v4w minP)
      (maximum $ map v4x maxP) (maximum $ map v4y maxP) (maximum $ map v4z maxP) (maximum $ map v4w maxP)
