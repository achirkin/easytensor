{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedSums                #-}
{-# LANGUAGE UnboxedTuples              #-}
module Numeric.DataFrame.Internal.Backend.Family.ScalarBase (ScalarBase (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial

-- | Specialize ScalarBase type without any arrays
newtype ScalarBase t = ScalarBase { _unScalarBase :: t }
  deriving ( Enum, Eq, Bounded, Integral
           , Num, Fractional, Floating, Ord, Real, RealFrac, RealFloat
           , PrimBytes)

instance Ord t => ProductOrder (ScalarBase t) where
  cmp a b = fromOrdering (compare (_unScalarBase a) (_unScalarBase b))
deriving instance Ord t => Ord (NonTransitive.ProductOrd (ScalarBase t))
deriving instance Ord t => Ord (Partial.ProductOrd (ScalarBase t))

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
  arrayContent# x = (# _unScalarBase x | #)
  {-# INLINE arrayContent# #-}
  offsetElems _ = 0#
  {-# INLINE offsetElems #-}
  uniqueOrCumulDims = Left . _unScalarBase
  {-# INLINE uniqueOrCumulDims #-}
  fromElems _ off ba = indexArray ba off
  {-# INLINE fromElems #-}

_suppressHlintUnboxedTuplesWarning :: () -> (# (), () #)
_suppressHlintUnboxedTuplesWarning = undefined
