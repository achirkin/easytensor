{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}
module Numeric.DataFrame.Internal.Backend.Family.ScalarBase (ScalarBase (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Backend.Family.PrimOps
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive                  as NonTransitive
import qualified Numeric.ProductOrd.Partial                        as Partial

-- | Specialize ScalarBase type without any arrays
newtype ScalarBase t = ScalarBase { _unScalarBase :: t }
  deriving ( Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat
           , PrimBytes)

instance Ord t => ProductOrder (ScalarBase t) where
  cmp a b = fromOrdering (compare (_unScalarBase a) (_unScalarBase b))
deriving instance Ord t => Ord (NonTransitive.ProductOrd (ScalarBase t))
deriving instance Ord t => Ord (Partial.ProductOrd (ScalarBase t))

instance Show t => Show (ScalarBase t) where
  show (ScalarBase t) = "{ " ++ show t ++ " }"


instance {-# OVERLAPPING #-} Bounded (ScalarBase Double) where
  maxBound = ScalarBase inftyD
  minBound = ScalarBase $ negate inftyD
instance {-# OVERLAPPING #-} Bounded (ScalarBase Float) where
  maxBound = ScalarBase inftyF
  minBound = ScalarBase $ negate inftyF
deriving instance {-# INCOHERENT #-} Bounded t => Bounded (ScalarBase t)

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
