{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
module Numeric.Quaternion.Internal
    ( Quaternion (..), Quater(Quater)
    ) where

import Numeric.Matrix.Internal (Matrix)
import Numeric.Vector.Internal (Vector)
import Text.Read

-- | @(x,y,z,w)@ of a quaternion
pattern Quater :: Quaternion t => t -> t -> t -> t -> Quater t
pattern Quater a b c d <- (unpackQ# -> (# a, b, c, d #))
  where
    Quater = packQ
{-# COMPLETE Quater #-}

-- | Quaternion operations
class Quaternion t where
    -- | Quaternion data type. The ordering of coordinates is @(x,y,z,w)@,
    --   where @w@ is the argument, and @x y z@ are the components of a 3D vector
    data Quater t
    -- | Set the quaternion in format @(x,y,z,w)@
    packQ :: t -> t -> t -> t -> Quater t
    -- | Get the values of the quaternion in format @(x,y,z,w)@
    unpackQ# :: Quater t -> (# t, t, t, t #)
    -- | Set the quaternion from 3D axis vector and argument
    fromVecNum :: Vector t 3 -> t -> Quater t
    -- | Set the quaternion from 4D vector in format @(x,y,z,w)@
    fromVec4 :: Vector t 4 -> Quater t
    -- | Transform the quaternion to 4D vector in format @(x,y,z,w)@
    toVec4 :: Quater t -> Vector t 4
    -- | Get scalar square of the quaternion.
    --
    --   >>> realToFrac (square q) == q * conjugate q
    square :: Quater t -> t
    -- | Imaginary part of the quaternion (orientation vector)
    im :: Quater t -> Quater t
    -- | Real part of the quaternion
    re :: Quater t -> Quater t
    -- | Imaginary part of the quaternion as a 3D vector
    imVec :: Quater t -> Vector t 3
    -- | Real part of the quaternion as a scalar
    taker :: Quater t -> t
    -- | i-th component
    takei :: Quater t -> t
    -- | j-th component
    takej :: Quater t -> t
    -- | k-th component
    takek :: Quater t -> t
    -- | Conjugate quaternion (negate imaginary part)
    conjugate :: Quater t -> Quater t
    -- | Rotates and scales vector in 3D using quaternion.
    --   Let \( q = c (\cos \frac{\alpha}{2}, v \sin \frac{\alpha}{2}) \)
    --     , \( c > 0 \), \( {|v|}^2 = 1 \);
    --   then the rotation angle is \( \alpha \), and the axis of rotation is \(v\).
    --   Scaling is proportional to \( c^2 \).
    --
    --   >>> rotScale q x == q * x * (conjugate q)
    rotScale :: Quater t -> Vector t 3 -> Vector t 3
    -- | Creates a quaternion @q@ from two vectors @a@ and @b@,
    --   such that @rotScale q a == b@.
    getRotScale :: Vector t 3 -> Vector t 3 -> Quater t
    -- | Creates a rotation versor from an axis vector and an angle in radians.
    --   Result is always a unit quaternion (versor).
    --   If the argument vector is zero, then result is a real unit quaternion.
    axisRotation :: Vector t 3 -> t -> Quater t
    -- | Quaternion rotation angle \( \alpha \)
    --   (where \( q = c (\cos \frac{\alpha}{2},  v \sin \frac{\alpha}{2}) \)
    --     , \( c > 0 \), \( {|v|}^2 = 1 \)).
    --
    --   >>> q /= 0 ==> axisRotation (imVec q) (qArg q) == signum q
    qArg :: Quater t -> t
    -- | Create a quaternion from a rotation matrix.
    --   Note, that rotations of \(q\) and \(-q\) are equivalent, there result of this
    --   function may be ambiguious. Assume the sign of the result to be chosen arbitrarily.
    fromMatrix33 :: Matrix t 3 3 -> Quater t
    -- | Create a quaternion from a homogenious coordinates trasform matrix.
    --   Ignores matrix translation transform.
    --   Note, that rotations of \(q\) and \(-q\) are equivalent, there result of this
    --   function may be ambiguious. Assume the sign of the result to be chosen arbitrarily.
    fromMatrix44 :: Matrix t 4 4 -> Quater t
    -- | Create a rotation matrix from a quaternion.
    --   Note, that rotations of \(q\) and \(-q\) are equivalent, so the following property holds:
    --
    --   >>> toMatrix33 q == toMatrix33 (-q)
    toMatrix33 :: Quater t -> Matrix t 3 3
    -- | Create a homogenious coordinates trasform matrix from a quaternion.
    --   Translation of the output matrix is zero.
    --   Note, that rotations of \(q\) and \(-q\) are equivalent, so the following property holds:
    --
    --   >>> toMatrix44 q == toMatrix44 (-q)
    toMatrix44 :: Quater t -> Matrix t 4 4


instance (Show t, Quaternion t, Ord t, Num t) => Show (Quater t) where
    showsPrec p (Quater x y z w)
        = case finS of
            SEmpty -> showChar '0'
            Simple -> finF
            SParen -> showParen (p > 6) finF
      where
        (finS, finF) = go SEmpty
          [(w, Nothing), (x, Just 'i'), (y, Just 'j'), (z, Just 'k')]
        go :: ShowState -> [(t, Maybe Char)] -> (ShowState, ShowS)
        go s ((v,l):xs)
          | (s0, f0) <- showComponent s v l
          , (s', f') <- go s0 xs
            = (s', f0 . f')
        go s [] = (s, id)
        showLabel Nothing  = id
        showLabel (Just c) = showChar c
        showComponent :: ShowState -> t -> Maybe Char -> (ShowState, ShowS)
        showComponent sState val mLabel = case (sState, compare val 0) of
          (_     , EQ) -> ( sState, id )
          (SEmpty, GT) -> ( Simple, shows val . showLabel mLabel )
          (SEmpty, LT) -> ( SParen, shows val . showLabel mLabel )
          (_     , GT) -> ( SParen
                          , showString " + " . shows val . showLabel mLabel )
          (_     , LT) -> ( SParen
                          , showString " - " . shows (negate val) . showLabel mLabel )


data ShowState = SEmpty | Simple | SParen
    deriving Eq

instance (Read t, Quaternion t, Num t) => Read (Quater t) where
    readPrec     = parens $ readPrec >>= go id 0 0 0 0
      where
        go :: (t -> t) -> t -> t -> t -> t -> t -> ReadPrec (Quater t)
        go f x y z w new =
          let def = pure (Quater x y z (f new))
              withLabel EOF         = def
              withLabel (Ident "i")
                = (lexP >>= proceed (f new) y z w) <++ pure (Quater (f new) y z w)
              withLabel (Ident "j")
                = (lexP >>= proceed x (f new) z w) <++ pure (Quater x (f new) z w)
              withLabel (Ident "k")
                = (lexP >>= proceed x y (f new) w) <++ pure (Quater x y (f new) w)
              withLabel l           = proceed x y z (f new) l
          in (lexP >>= withLabel) <++ def
        proceed :: t -> t -> t -> t -> Lexeme -> ReadPrec (Quater t)
        proceed x y z w (Symbol "+") = readPrec >>= go id x y z w
        proceed x y z w (Symbol "-") = readPrec >>= go negate x y z w
        proceed x y z w EOF          = pure (Quater x y z w)
        proceed _ _ _ _ _            = pfail

    readListPrec = readListPrecDefault
    readList     = readListDefault
