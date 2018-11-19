{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Numeric.DataFrame.Internal.Array.Family.ScalarBase (ScalarBase (..)) where


import           GHC.Base
import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import {-# SOURCE #-} Numeric.DataFrame.Internal.Array.Family  (Array)
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.PrimBytes

-- | Specialize ScalarBase type without any arrays
newtype ScalarBase t = ScalarBase { _unScalarBase :: t }
  deriving ( Enum, Eq, Integral
           , Num, Fractional, Floating, Ord, Read, Real, RealFrac, RealFloat)

-- instance {-# OVERLAPPING #-} Show t => Show (ScalarBase t) where
--   show (ScalarBase t) = "{ " ++ show t ++ " }"

instance {-# OVERLAPPING #-}
    Array Double '[] ~ ScalarBase Double =>  Bounded (DataFrame Double ('[] :: [Nat])) where
    maxBound = unsafeCoerce# (ScalarBase inftyD)
    minBound = unsafeCoerce# (ScalarBase $ negate inftyD)
instance {-# OVERLAPPING #-}
    Array Float '[] ~ ScalarBase Float => Bounded (DataFrame Float ('[] :: [Nat])) where
    maxBound = unsafeCoerce# (ScalarBase inftyF)
    minBound = unsafeCoerce# (ScalarBase $ negate inftyF)
instance {-# OVERLAPS #-}
    (Array t '[] ~ ScalarBase t, Bounded t) => Bounded (DataFrame t ('[] :: [Nat])) where
    maxBound = unsafeCoerce# (ScalarBase maxBound :: Array t '[])
    minBound = unsafeCoerce# (ScalarBase minBound :: Array t '[])

deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Enum t) => Enum (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Eq t) => Eq (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Integral t) => Integral (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Num t) => Num (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Fractional t) => Fractional (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Floating t) => Floating (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Ord t) => Ord (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Read t) => Read (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, Real t) => Real (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, RealFrac t) => RealFrac (DataFrame t ('[] :: [Nat]))
deriving instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, RealFloat t) => RealFloat (DataFrame t ('[] :: [Nat]))

instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, PrimBytes t) => PrimBytes (DataFrame t ('[] :: [Nat])) where
    getBytes = unsafeCoerce# (getBytes @t)
    {-# INLINE getBytes #-}
    fromBytes = unsafeCoerce# (fromBytes @t)
    {-# INLINE fromBytes #-}
    readBytes = unsafeCoerce# (readBytes @t)
    {-# INLINE readBytes #-}
    writeBytes = unsafeCoerce# (writeBytes @t)
    {-# INLINE writeBytes #-}
    readAddr = unsafeCoerce# (readAddr @t)
    {-# INLINE readAddr #-}
    writeAddr = unsafeCoerce# (writeAddr @t)
    {-# INLINE writeAddr #-}
    byteSize = unsafeCoerce# (byteSize @t)
    {-# INLINE byteSize #-}
    byteAlign = unsafeCoerce# (byteAlign @t)
    {-# INLINE byteAlign #-}
    byteOffset = unsafeCoerce# (byteOffset @t)
    {-# INLINE byteOffset #-}
    indexArray = unsafeCoerce# (indexArray @t)
    {-# INLINE indexArray #-}
    readArray = unsafeCoerce# (readArray @t)
    {-# INLINE readArray #-}
    writeArray = unsafeCoerce# (writeArray @t)
    {-# INLINE writeArray #-}

instance {-# OVERLAPPING #-} (Array t '[] ~ ScalarBase t, PrimBytes t) => PrimArray t (DataFrame t ('[] :: [Nat])) where
    broadcast = _toDF#
    {-# INLINE broadcast #-}
    ix# _ = _unDF#
    {-# INLINE ix# #-}
    gen# _ = unsafeCoerce#
    {-# INLINE gen# #-}
    upd# _ 0# = const . _toDF#
    upd# _ _  = const id
    {-# INLINE upd# #-}
    elemOffset _ = 0#
    {-# INLINE elemOffset #-}
    elemSize0 _ = 1#
    {-# INLINE elemSize0 #-}
    fromElems off _ ba = indexArray ba off
    {-# INLINE fromElems #-}

_toDF# :: t -> DataFrame t ('[] :: [Nat])
_toDF# = unsafeCoerce#
{-# INLINE _toDF# #-}

_unDF# :: DataFrame t ('[] :: [Nat]) -> t
_unDF# = unsafeCoerce#
{-# INLINE _unDF# #-}
