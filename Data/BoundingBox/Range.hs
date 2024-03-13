{- |
  This module provides the 'Range' type and several functions for working with ranges.
-}

module Data.BoundingBox.Range where

import Data.Vector.Class

{- |
  A 'Range' represents a continuous interval between two 'Scalar' endpoints.
-}
data Range = Range {min_point, max_point :: {-# UNPACK #-} !Scalar} deriving (Eq, Show)

-- | Given two 'Scalar's, construct a 'Range' (swapping the endpoints if necessary so that they are in the correct order.
bound_corners :: Scalar -> Scalar -> Range
bound_corners xa xb = Range (min xa xb) (max xa xb)

-- | Find the bounds of a list of points. (Throws an exception if the list is empty.)
bound_points :: [Scalar] -> Range
bound_points xs = Range (minimum xs) (maximum xs)

-- | Test whether a given 'Scalar' falls within a particular 'Range'.
within_bounds :: Scalar -> Range -> Bool
within_bounds x (Range x0 x1) = x0 <= x && x <= x1

-- | Take the union of two ranges. The resulting 'Range' contains all points that the original ranges contained, plus any points between them (if the original ranges don't overlap).
union :: Range -> Range -> Range
union (Range ll lh) (Range rl rh) = Range (min ll rl) (max lh rh)

-- | Take the intersection of two ranges. If the ranges do not overlap, the intersection is empty, and 'Nothing' is returned. (This is a good way to check whether two ranges overlap or not.) Otherwise a new 'Range' is returned that contains only the points common to both ranges.
isect :: Range -> Range -> Maybe Range
isect (Range ll lh) (Range rl rh) =
  let
    nl = max ll rl
    nh = min lh rh
  in  if nl > nh then Nothing else Just (Range nl nh)

-- | Efficiently compute the union of a list of ranges.
unions :: [Range] -> Range
unions rs = Range (minimum $ map min_point rs) (maximum $ map max_point rs)
