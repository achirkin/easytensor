{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
module Numeric.DataFrame.Internal.Array.Family.ScalarBase (ScalarBase (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.PrimBytes

-- | Specialize ScalarBase type without any arrays
newtype ScalarBase t = ScalarBase { _unScalarBase :: t }
  deriving ( Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes)

instance Show t => Show (ScalarBase t) where
  show (ScalarBase t) = "{ " ++ show t ++ " }"

deriving instance {-# OVERLAPPABLE #-} Bounded t => Bounded (ScalarBase t)
instance {-# OVERLAPPING #-} Bounded (ScalarBase Double) where
  maxBound = ScalarBase inftyD
  minBound = ScalarBase $ negate inftyD
instance {-# OVERLAPPING #-} Bounded (ScalarBase Float) where
  maxBound = ScalarBase inftyF
  minBound = ScalarBase $ negate inftyF

instance PrimBytes t => PrimArray t (ScalarBase t) where
  broadcast = unsafeCoerce#
  {-# INLINE broadcast #-}
  ix# _ = unsafeCoerce#
  {-# INLINE ix# #-}
  gen# _ = unsafeCoerce#
  {-# INLINE gen# #-}
  upd# _ 0# = const . ScalarBase
  upd# _ _  = const id
  {-# INLINE upd# #-}
  offsetElems _ = 0#
  {-# INLINE offsetElems #-}
  uniqueOrCumulDims = Left . _unScalarBase
  {-# INLINE uniqueOrCumulDims #-}
  fromElems _ off ba = indexArray ba off
  {-# INLINE fromElems #-}

_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
