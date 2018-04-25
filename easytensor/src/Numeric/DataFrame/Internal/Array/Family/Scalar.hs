{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UnboxedTuples              #-}
module Numeric.DataFrame.Internal.Array.Family.Scalar (Scalar (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.Internal
import           Numeric.PrimBytes

-- | Specialize scalar type without any arrays
newtype Scalar k t = Scalar { _unScalar :: t }
  deriving ( Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes)

instance Show t => Show (Scalar k t) where
  show (Scalar t) = "{ " ++ show t ++ " }"

deriving instance {-# OVERLAPPABLE #-} Bounded t => Bounded (Scalar k t)
instance {-# OVERLAPPING #-} Bounded (Scalar k Double) where
  maxBound = Scalar inftyD
  minBound = Scalar $ negate inftyD
instance {-# OVERLAPPING #-} Bounded (Scalar k Float) where
  maxBound = Scalar inftyF
  minBound = Scalar $ negate inftyF

instance PrimBytes t => PrimArray t (Scalar k t) where
  broadcast = unsafeCoerce#
  {-# INLINE broadcast #-}
  ix# _ = unsafeCoerce#
  {-# INLINE ix# #-}
  gen# _ = unsafeCoerce#
  {-# INLINE gen# #-}
  upd# _ 0# = const . Scalar
  upd# _ _  = const id
  {-# INLINE upd# #-}
  elemOffset _ = 0#
  {-# INLINE elemOffset #-}
  elemSize0 _ = 1#
  {-# INLINE elemSize0 #-}
  fromElems off _ ba = indexArray ba off
  {-# INLINE fromElems #-}

_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
