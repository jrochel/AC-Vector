{- |
  General functions applicable to all vector types.
-}

module Data.Vector.Class where

-- | The type of vector field values.
type Scalar = Double

{- |
  All vector types belong to this class. Aside from 'vpack' and 'vunpack', these methods aren't especially useful to end-users; they're used internally by the vector arithmetic implementations.
-}
class BasicVector v where
  -- | Apply a function to all vector fields.
  vmap    :: (Scalar -> Scalar) -> (v -> v)

  -- | Zip two vectors together field-by-field using the supplied function (in the style of @Data.List.zipWith@).
  vzip    :: (Scalar -> Scalar -> Scalar) -> (v -> v -> v)

  -- | Reduce a vector down to a single value using the supplied binary operator. The ordering in which this happens isn't guaranteed, so the operator should probably be associative and commutative.
  vfold   :: (Scalar -> Scalar -> Scalar) -> (v -> Scalar)

  -- | Pack a list of values into a vector. Extra values are ignored, too few values yields @Nothing@.
  vpack   :: [Scalar] -> Maybe v

  -- | Unpack a vector into a list of values. (Always succeeds.)
  vunpack :: v -> [Scalar]

  -- | Convert a 'Scalar' to a vector (with all components the same).
  vpromote :: Scalar -> v

{- |
  Dummy class that enables you to request a vector in a type signature without needing to explicitly list 'Num' or 'Fractional' as well.
-}
class (BasicVector v, Num v, Fractional v) => Vector v where

{- |
  Scale a vector (i.e., change its length but not its direction). This operator has the same precedence as the usual @(*)@ operator.

  The @(*|)@ and @(|*)@ operators are identical, but with their argument flipped. Just remember that the \'@|@\' denotes the scalar part.
-}
(*|) :: Vector v => Scalar -> v -> v
k *| v = vmap (k*) v

{- |
  Scale a vector (i.e., change its length but not its direction). This operator has the same precedence as the usual @(*)@ operator.

  The @(*|)@ and @(|*)@ operators are identical, but with their argument flipped. Just remember that the \'@|@\' denotes the scalar part.
-}
(|*) :: Vector v => v -> Scalar -> v
v |* k = vmap (k*) v

{- |
  Scale a vector (i.e., change its length but not its direction). This operator has the same precedence as the usual @(/)@ operator.

  The @(/|)@ and @(|/)@ operators are identical, but with their argument flipped. Just remember that the \'@|@\' denotes the scalar part.
-}
(|/) :: Vector v => v -> Scalar -> v
v |/ k = v |* (1/k)

{- |
  Scale a vector (i.e., change its length but not its direction). This operator has the same precedence as the usual @(/)@ operator.

  The @(/|)@ and @(|/)@ operators are identical, but with their argument flipped. Just remember that the \'@|@\' denotes the scalar part.
-}
(/|) :: Vector v => Scalar -> v -> v
k /| v = (1/k) *| v

infixl 7 *|
infixl 7 |*
infixl 7 /|
infixl 7 |/

-- | Take the /dot product/ of two vectors. This is a scalar equal to the cosine of the angle between the two vectors multiplied by the length of each vectors.
vdot :: Vector v => v -> v -> Scalar
v1 `vdot` v2 = vfold (+) $ vzip (*) v1 v2

-- | Return the length or /magnitude/ of a vector. (Note that this involves a slow square root operation.)
vmag :: Vector v => v -> Scalar
vmag v = sqrt (v `vdot` v)

-- | Normalise a vector. In order words, return a new vector with the same direction, but a length of exactly one. (If the vector's length is zero or very near to zero, the vector is returned unchanged.)
vnormalise :: Vector v => v -> v
vnormalise v =
  let m = vmag v
  in  if m < 1e-16 then v else v |* (1/m)

{- |
  Linearly interpolate between two points in space.

  * @vlinear 0 a b = a@

  * @vlinear 1 a b = b@

  * @vlinear 0.5 a b@ would give a point exactly half way between @a@ and @b@ in a straight line.
-}
vlinear :: (Vector v) => Scalar -> v -> v -> v
vlinear t a b = (1-t) *| a + t *| b
