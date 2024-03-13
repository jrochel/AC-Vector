{- |
  This module provides the 'BBox3' type for 3-dimensional bounding boxes (\"bounding volumes\").
-}

module Data.BoundingBox.B3 where

import Data.Vector.Class
import Data.Vector.V3
import qualified Data.BoundingBox.Range as R

-- | A 'BBox3' is a 3D bounding box (aligned to the coordinate axies).
data BBox3 = BBox3 {minX, minY, minZ, maxX, maxY, maxZ :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

-- | Return the X-range that this bounding box covers.
rangeX :: BBox3 -> R.Range
rangeX b = R.Range (minX b) (maxX b)

-- | Return the Y-range that this bounding box covers.
rangeY :: BBox3 -> R.Range
rangeY b = R.Range (minY b) (maxY b)

-- | Return the Z-range that this bounding box covers.
rangeZ :: BBox3 -> R.Range
rangeZ b = R.Range (minZ b) (maxZ b)

-- | Given ranges for each coordinate axis, construct a bounding box.
rangeXYZ :: R.Range -> R.Range -> R.Range -> BBox3
rangeXYZ (R.Range x0 x1) (R.Range y0 y1) (R.Range z0 z1) = BBox3 x0 y0 z0 x1 y1 z1

-- | Given a pair of corner points, construct a bounding box. (The points must be from opposite corners, but it doesn't matter /which/ corners nor which order they are given in.)
bound_corners :: Vector3 -> Vector3 -> BBox3
bound_corners (Vector3 xa ya za) (Vector3 xb yb zb) = BBox3 (min xa xb) (min ya yb) (min za zb) (max xa xb) (max ya yb) (max za zb)

-- | Find the bounds of a list of points. (Throws an exception if the list is empty.)
bound_points :: [Vector3] -> BBox3
bound_points ps =
  let
    xs = map v3x ps
    ys = map v3y ps
    zs = map v3z ps
  in BBox3 (minimum xs) (minimum ys) (minimum zs) (maximum xs) (maximum ys) (maximum zs)

-- | Test whether a given 3D vector is inside this bounding box.
within_bounds :: Vector3 -> BBox3 -> Bool
within_bounds (Vector3 x y z) b =
  x `R.within_bounds` (rangeX b) &&
  y `R.within_bounds` (rangeY b) &&
  z `R.within_bounds` (rangeZ b)

-- | Return the minimum values for all coordinates.
min_point :: BBox3 -> Vector3
min_point (BBox3 x0 y0 z0 x1 y1 z1) = Vector3 x0 y0 z0

-- | Return the maximum values for all coordinates.
max_point :: BBox3 -> Vector3
max_point (BBox3 x0 y0 z0 x1 y1 z1) = Vector3 x1 y1 z1

-- | Take the union of two bounding boxes. The result is a new bounding box that contains all the points the original boxes contained, plus any extra space between them.
union :: BBox3 -> BBox3 -> BBox3
union b0 b1 =
  let
    rx = (rangeX b0) `R.union` (rangeX b1)
    ry = (rangeY b0) `R.union` (rangeY b1)
    rz = (rangeZ b0) `R.union` (rangeZ b1)
  in rangeXYZ rx ry rz

-- | Take the intersection of two bounding boxes. If the boxes do not overlap, return 'Nothing'. Otherwise return a new bounding box containing only the points common to both argument boxes.
isect :: BBox3 -> BBox3 -> Maybe BBox3
isect b0 b1 = do
  rx <- (rangeX b0) `R.isect` (rangeX b1)
  ry <- (rangeY b0) `R.isect` (rangeY b1)
  rz <- (rangeZ b0) `R.isect` (rangeZ b1)
  return (rangeXYZ rx ry rz)

-- | Efficiently compute the union of a list of bounding boxes.
unions :: [BBox3] -> BBox3
unions bs =
  let
    minP = map min_point bs
    maxP = map max_point bs
  in
    BBox3
      (minimum $ map v3x minP) (minimum $ map v3y minP) (minimum $ map v3z minP)
      (maximum $ map v3x maxP) (maximum $ map v3y maxP) (maximum $ map v3z maxP)
