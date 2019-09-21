{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Numeric.Basics
  ( -- * Constants
    pattern M_E, pattern M_LOG2E, pattern M_LOG10E, pattern M_LN2, pattern M_LN10
  , pattern M_SQRT2, pattern M_SQRT1_2
  , pattern M_PI, pattern M_PI_2, pattern M_PI_3, pattern M_PI_4
  , pattern M_1_PI, pattern M_2_PI, pattern M_2_SQRTPI
  , Epsilon (..), pattern M_EPS
    -- * Functions
  , RealExtras (..), RealFloatExtras (..)
  , negateUnless
  ) where

import Data.Int
import Data.Word
import Numeric.PrimBytes

-- | \( e \)
pattern M_E :: (Eq a, Fractional a) => Fractional a => a
pattern M_E = 2.718281828459045235360287471352662498

-- | \( \log_2 e \)
pattern M_LOG2E :: (Eq a, Fractional a) => Fractional a => a
pattern M_LOG2E = 1.442695040888963407359924681001892137

-- | \( \log_{10} e \)
pattern M_LOG10E :: (Eq a, Fractional a) => Fractional a => a
pattern M_LOG10E = 0.434294481903251827651128918916605082

-- | \( \log_e 2 \)
pattern M_LN2 :: (Eq a, Fractional a) => Fractional a => a
pattern M_LN2 = 0.693147180559945309417232121458176568

-- | \( \log_e 10 \)
pattern M_LN10 :: (Eq a, Fractional a) => Fractional a => a
pattern M_LN10 = 2.302585092994045684017991454684364208

-- | \( \sqrt{2} \)
pattern M_SQRT2 :: (Eq a, Fractional a) => Fractional a => a
pattern M_SQRT2 = 1.414213562373095048801688724209698079

-- | \( \frac{1}{\sqrt{2}} \)
pattern M_SQRT1_2 :: (Eq a, Fractional a) => Fractional a => a
pattern M_SQRT1_2 = 0.707106781186547524400844362104849039

-- | \( \pi \)
pattern M_PI :: (Eq a, Fractional a) => Fractional a => a
pattern M_PI = 3.1415926535897932384626433832795028841971
-- | \( \frac{\pi}{2} \)
pattern M_PI_2 :: (Eq a, Fractional a) => Fractional a => a
pattern M_PI_2 = 1.570796326794896619231321691639751442

-- | \( \frac{\pi}{3} \)
pattern M_PI_3 :: (Eq a, Fractional a) => Fractional a => a
pattern M_PI_3 = 1.047197551196597746154214461093167628

-- | \( \frac{\pi}{4} \)
pattern M_PI_4 :: (Eq a, Fractional a) => Fractional a => a
pattern M_PI_4 = 0.785398163397448309615660845819875721

-- | \( \frac{1}{\pi} \)
pattern M_1_PI :: (Eq a, Fractional a) => Fractional a => a
pattern M_1_PI = 0.318309886183790671537767526745028724

-- | \( \frac{2}{\pi} \)
pattern M_2_PI :: (Eq a, Fractional a) => Fractional a => a
pattern M_2_PI = 0.636619772367581343075535053490057448

-- | \( \frac{2}{\sqrt{\pi}} \)
pattern M_2_SQRTPI :: (Eq a, Fractional a) => Fractional a => a
pattern M_2_SQRTPI = 1.128379167095512573896158903121545172

-- | Define a meaningful precision for complex floating-point operations.
class (Eq a, Floating a) => Epsilon a where
    -- | A small positive number that depends on the type of @a@.
    --   Compare your values to @epsilon@ to tell if they are near zero.
    epsilon :: a

instance Epsilon Double where
    epsilon = 1e-12

instance Epsilon Float where
    epsilon = 1e-6

-- | A small positive number that depends on the type of @a@.
pattern M_EPS :: Epsilon a => a
pattern M_EPS <- ((epsilon ==) -> True)
  where
    M_EPS = epsilon

-- | Negate if @False@.
--   This is a useful alternative to `signum`,
--     when @signum 0 == 0@ causing some troubles.
negateUnless :: Num t => Bool -> t -> t
negateUnless True  = id
negateUnless False = negate
{-# INLINE negateUnless #-}

-- | Extra functions for `Real` types.
class (Real a, PrimBytes a) => RealExtras a where
    -- | @copysign x y@ returns a value with the magnitude of x and the sign of y.
    --
    --   NB: in future, this function is to reimplemented using primops
    --       and should behave the same way as its C analogue.
    copysign :: a -> a -> a
    copysign x y
      | (x >= 0) == (y >= 0) = x
      | otherwise = negate x
    {-# INLINE copysign #-}

instance RealExtras Int
instance RealExtras Int8
instance RealExtras Int16
instance RealExtras Int32
instance RealExtras Int64
instance RealExtras Word
instance RealExtras Word8
instance RealExtras Word16
instance RealExtras Word32
instance RealExtras Word64

instance RealExtras Float where
    copysign = c'copysignf
    {-# INLINE copysign #-}

instance RealExtras Double where
    copysign = c'copysignd
    {-# INLINE copysign #-}

-- | Extra functions for `RealFrac` types.
class (Epsilon a, RealExtras a, RealFloat a) => RealFloatExtras a where
    -- | \( \sqrt{ x^2 + y^2 } \).
    --
    --   NB: in future, this function is to reimplemented using primops
    --       and should behave the same way as its C analogue.
    hypot :: a -> a -> a
    hypot x y = scaleFloat ea (sqrt (an*an + bn*bn))
      where
        x' = abs x
        y' = abs y
        (a,b) = if x' >= y' then (x', y') else (y', x')
        (_, ea) = decodeFloat a
        an = scaleFloat (negate ea) a
        bn = scaleFloat (negate ea) b
    {-# INLINE hypot #-}
    -- | Maximum finite number representable by this FP type.
    maxFinite :: a

instance RealFloatExtras Float where
    hypot = c'hypotf
    {-# INLINE hypot #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}


instance RealFloatExtras Double where
    hypot = c'hypotd
    {-# INLINE hypot #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}

foreign import ccall unsafe "hypot"  c'hypotd :: Double -> Double -> Double
foreign import ccall unsafe "hypotf" c'hypotf :: Float -> Float -> Float
foreign import ccall unsafe "copysign"  c'copysignd :: Double -> Double -> Double
foreign import ccall unsafe "copysignf" c'copysignf :: Float -> Float -> Float
