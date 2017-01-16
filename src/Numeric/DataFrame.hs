{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeInType #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, MagicHash #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, FunctionalDependencies     #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame
  ( DataFrame
  ) where

import GHC.Types (Type)
import           Control.Arrow        (first, second)
import           GHC.TypeLits         (Nat)
import           Numeric.Array
import           Numeric.Commons
import           Numeric.Dimensions
import           Text.Read
import           Unsafe.Coerce
import           Data.Type.Equality

-- | Keep data in a primitive data frame
--    and maintain information about dimensions in the type-system
data DataFrame :: (Type -> [k] -> Type) where
   DataFrameKnown :: forall t ns
                   . Dimensions (KnownDims1 ns)
                  => Array t (KnownDims1 ns) -> DataFrame t ns
   DataFrameSome  :: forall t ns xns
                   . (Dimensions ns, FixedDim xns ns)
                  => Dim xns -> Array t ns -> DataFrame t xns


type family KnownDims1 (x::[k]) = (y :: [Nat]) | y -> x where
  KnownDims1 ('[]::[Nat]) = '[]
  KnownDims1 (n ': ns) = n ': ns


wrapKnown :: Dimensions ns => Array t ns -> DataFrame t ns
wrapKnown a = case f a of
  Refl -> DataFrameKnown a
  where
    f :: a ns -> ns :~: KnownDims1 ns
    f _ = unsafeCoerce Refl

instance ( Show (Array t (KnownDims1 ds))
         , Show (Dim ds)
         ) => Show (DataFrame t (ds :: [Nat])) where
  show (DataFrameKnown arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show (dim `inSpaceOf` arr)
                         ++ "\n\tContent:\n" ++ show arr


-- instance ( Show (Dim ds)
--          ) => Show (DataFrame t (ds :: [XNat])) where
--   show x@(DataFrame ds arr) = "DataFrame:"
--                          ++ "\n\tShape: " ++ show ds
--                          ++ "\n\tContent:\n" ++
--     ( withShape x show
--     )
--
--
-- instance PreservingDim (DataFrame t) (DataFrame t) where
--   shape x@(DataFrameKnown a) = case unsafeProof x a of
--     Refl -> dim
--   shape (DataFrameSome ds _) = ds
--   looseDims x@(DataFrameKnown a)
--       = case (unsafeProof x a, unsafeIsFixed d a) of
--         (Refl, Refl) -> DataFrameSome d a
--     where
--       d = wrapDims (dim `inSpaceOf` a)
--       wrapDims :: Dim ns -> Dim (WrapNats ns)
--       wrapDims D         = D
--       wrapDims (n :* ns) = n :* wrapDims ns
--   looseDims (DataFrameSome _ _) = error
--     "Something is wrong: DataFrameSome should be parameterized by XNat."
--   withShape (DataFrameSome _ a) f = f (wrapKnown a)
--   withShape (DataFrameKnown _) _ = error
--     "Something is wrong: DataFrameKnown should be parameterized by Nat."

--
-- instance ( Bounded (Array t ds)
--          , Dimensions' ds
--          ) => Bounded (DataFrame t ds) where
--   minBound = DataFrame dim minBound
--   {-# INLINE minBound #-}
--   maxBound = DataFrame dim maxBound
--   {-# INLINE maxBound #-}
--
-- instance ( Enum (Array t ds)
--          , Dimensions' ds
--          ) => Enum (DataFrame t ds) where
--   succ = mapV succ
--   {-# INLINE succ #-}
--   pred = mapV pred
--   {-# INLINE pred #-}
--   toEnum = DataFrame dim . toEnum
--   {-# INLINE toEnum #-}
--   fromEnum (DataFrame _ x) = fromEnum x
--   {-# INLINE fromEnum #-}
--   enumFrom (DataFrame d x) = DataFrame d <$> enumFrom x
--   {-# INLINE enumFrom #-}
--   enumFromTo (DataFrame d x) (DataFrame _ y)
--     = DataFrame d <$> enumFromTo x y
--   {-# INLINE enumFromTo #-}
--   enumFromThen (DataFrame d x) (DataFrame _ x')
--     = DataFrame d <$> enumFromThen x x'
--   {-# INLINE enumFromThen #-}
--   enumFromThenTo (DataFrame d x) (DataFrame _ x') (DataFrame _ y)
--     = DataFrame d <$> enumFromThenTo x x' y
--   {-# INLINE enumFromThenTo #-}
--
-- instance Eq (Array t ds) => Eq (DataFrame t ds) where
--   DataFrame ds1 arr1 == DataFrame ds2 arr2 = ds1 == ds2 && arr1 == arr2
--   {-# INLINE (==) #-}
--   DataFrame ds1 arr1 /= DataFrame ds2 arr2 = ds1 /= ds2 || arr1 /= arr2
--   {-# INLINE (/=) #-}
--
-- instance ( Integral (Array t ds)
--          , Real (DataFrame t ds)
--          , Enum (DataFrame t ds)
--          ) => Integral (DataFrame t ds) where
--   quot = zipV quot
--   {-# INLINE quot #-}
--   rem = zipV rem
--   {-# INLINE rem #-}
--   div = zipV div
--   {-# INLINE div #-}
--   mod = zipV mod
--   {-# INLINE mod #-}
--   quotRem (DataFrame d x) (DataFrame _ y) = case quotRem x y of
--     (a,b) -> (DataFrame d a, DataFrame d b)
--   {-# INLINE quotRem #-}
--   divMod (DataFrame d x) (DataFrame _ y) = case divMod x y of
--     (a,b) -> (DataFrame d a, DataFrame d b)
--   {-# INLINE divMod #-}
--   toInteger (DataFrame _ x) = toInteger x
--   {-# INLINE toInteger #-}
--
-- -- | Implement partial ordering for `>`, `<`, `>=`, `<=`
-- --     and lexicographical ordering for `compare`
-- instance Ord (Array t ds) => Ord (DataFrame t ds) where
--   (>) = combineV (>)
--   {-# INLINE (>) #-}
--   (<) = combineV (<)
--   {-# INLINE (<) #-}
--   (>=) = combineV (>=)
--   {-# INLINE (>=) #-}
--   (<=) = combineV (<=)
--   {-# INLINE (<=) #-}
--   -- | Compare lexicographically
--   compare = combineV compare
--   {-# INLINE compare #-}
--   -- | Element-wise minimum
--   min = zipV min
--   {-# INLINE min #-}
--   -- | Element-wise maximum
--   max = zipV max
--   {-# INLINE max #-}
--
--
-- instance ( Num (Array t ds)
--          , Dimensions' ds
--          ) => Num (DataFrame t ds) where
--   (+) = zipV (+)
--   {-# INLINE (+) #-}
--   (-) = zipV (-)
--   {-# INLINE (-) #-}
--   (*) = zipV (*)
--   {-# INLINE (*) #-}
--   negate = mapV negate
--   {-# INLINE negate #-}
--   abs = mapV abs
--   {-# INLINE abs #-}
--   signum = mapV signum
--   {-# INLINE signum #-}
--   fromInteger = DataFrame dim . fromInteger
--   {-# INLINE fromInteger #-}
--
-- instance ( Fractional (Array t ds)
--          , Dimensions' ds
--          ) => Fractional (DataFrame t ds) where
--   (/) = zipV (/)
--   {-# INLINE (/) #-}
--   recip = mapV recip
--   {-# INLINE recip #-}
--   fromRational = DataFrame dim . fromRational
--   {-# INLINE fromRational #-}
--
--
-- instance ( Floating (Array t ds)
--          , Dimensions' ds
--          ) => Floating (DataFrame t ds) where
--   pi = DataFrame dim pi
--   {-# INLINE pi #-}
--   exp = mapV exp
--   {-# INLINE exp #-}
--   log = mapV log
--   {-# INLINE log #-}
--   sqrt = mapV sqrt
--   {-# INLINE sqrt #-}
--   sin = mapV sin
--   {-# INLINE sin #-}
--   cos = mapV cos
--   {-# INLINE cos #-}
--   tan = mapV tan
--   {-# INLINE tan #-}
--   asin = mapV asin
--   {-# INLINE asin #-}
--   acos = mapV acos
--   {-# INLINE acos #-}
--   atan = mapV atan
--   {-# INLINE atan #-}
--   sinh = mapV sinh
--   {-# INLINE sinh #-}
--   cosh = mapV cosh
--   {-# INLINE cosh #-}
--   tanh = mapV tanh
--   {-# INLINE tanh #-}
--   (**) = zipV (**)
--   {-# INLINE (**) #-}
--   logBase = zipV logBase
--   {-# INLINE logBase #-}
--   asinh = mapV asinh
--   {-# INLINE asinh #-}
--   acosh = mapV acosh
--   {-# INLINE acosh #-}
--   atanh = mapV atanh
--   {-# INLINE atanh #-}
--
-- instance ( Read (Array t ds)
--          , Dimensions' ds
--          ) => Read (DataFrame t ds) where
--   readsPrec n =  map (first (DataFrame dim)) . readsPrec n
--   {-# INLINE readsPrec #-}
--   readList = map (first (map $ DataFrame dim)) . readList
--   {-# INLINE readList #-}
--   readPrec = DataFrame dim <$> readPrec
--   {-# INLINE readPrec #-}
--   readListPrec = map (DataFrame dim) <$> readListPrec
--   {-# INLINE readListPrec #-}
--
-- instance ( Real (Array t ds)
--          , Num (DataFrame t ds)
--          ) => Real (DataFrame t ds) where
--   toRational (DataFrame _ x) = toRational x
--   {-# INLINE toRational #-}
--
--
-- instance ( RealFrac (Array t ds)
--          , Real (DataFrame t ds)
--          , Fractional (DataFrame t ds)
--          ) => RealFrac (DataFrame t ds) where
--   properFraction (DataFrame d x) = second (DataFrame d) $ properFraction x
--   {-# INLINE properFraction #-}
--   truncate (DataFrame _ x) = truncate x
--   {-# INLINE truncate #-}
--   round (DataFrame _ x) = round x
--   {-# INLINE round #-}
--   ceiling (DataFrame _ x) = ceiling x
--   {-# INLINE ceiling #-}
--   floor (DataFrame _ x) = floor x
--   {-# INLINE floor #-}
--
-- instance ( RealFloat (Array t ds)
--          , RealFrac (DataFrame t ds)
--          , Floating (DataFrame t ds)
--          , Dimensions' ds
--          ) => RealFloat (DataFrame t ds) where
--    floatRadix = floatRadix . dData
--    {-# INLINE floatRadix #-}
--    floatDigits = floatDigits . dData
--    {-# INLINE floatDigits #-}
--    floatRange = floatRange . dData
--    {-# INLINE floatRange #-}
--    decodeFloat = decodeFloat . dData
--    {-# INLINE decodeFloat #-}
--    encodeFloat i = DataFrame dim . encodeFloat i
--    {-# INLINE encodeFloat #-}
--    exponent = exponent . dData
--    {-# INLINE exponent #-}
--    significand = mapV significand
--    {-# INLINE significand #-}
--    scaleFloat i = mapV (scaleFloat i)
--    {-# INLINE scaleFloat #-}
--    isNaN = isNaN  . dData
--    {-# INLINE isNaN #-}
--    isInfinite = isInfinite . dData
--    {-# INLINE isInfinite #-}
--    isDenormalized = isDenormalized . dData
--    {-# INLINE isDenormalized #-}
--    isNegativeZero = isNegativeZero . dData
--    {-# INLINE isNegativeZero #-}
--    isIEEE = isIEEE . dData
--    {-# INLINE isIEEE #-}
--    atan2 = zipV atan2
--    {-# INLINE atan2 #-}
--
--
-- instance ( PrimBytes (Array t ds)
--          , Dimensions' ds
--          ) => PrimBytes (DataFrame t ds) where
--   toBytes x = toBytes (dData x)
--   {-# INLINE toBytes #-}
--   fromBytes b = DataFrame dim (fromBytes b)
--   {-# INLINE fromBytes #-}
--   byteSize x = byteSize (dData x)
--   {-# INLINE byteSize #-}
--   byteAlign x = byteAlign (dData x)
--   {-# INLINE byteAlign #-}
--   elementByteSize x = elementByteSize (dData x)
--   {-# INLINE elementByteSize #-}
--
-- instance ( FloatBytes (Array t ds)
--          ) => FloatBytes (DataFrame t ds) where
--   ixF i x = ixF i (dData x)
-- instance ( DoubleBytes (Array t ds)
--          ) => DoubleBytes (DataFrame t ds) where
--   ixD i x = ixD i (dData x)
-- instance ( IntBytes (Array t ds)
--          ) => IntBytes (DataFrame t ds) where
--   ixI i x = ixI i (dData x)
-- instance ( WordBytes (Array t ds)
--          ) => WordBytes (DataFrame t ds) where
--   ixW i x = ixW i (dData x)
--
--
-- combineV :: (Array t ds -> Array t ds -> a)
--          -> DataFrame t ds -> DataFrame t ds -> a
-- combineV f (DataFrame _ arr1) (DataFrame _ arr2) = f arr1 arr2
-- {-# INLINE combineV #-}
--
-- zipV :: (Array t ds -> Array t ds -> Array t ds)
--      -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
-- zipV f (DataFrame d arr1) (DataFrame _ arr2) = DataFrame d (f arr1 arr2)
-- {-# INLINE zipV #-}
--
-- mapV :: (Array t ds -> Array t ds)
--      -> DataFrame t ds -> DataFrame t ds
-- mapV f (DataFrame d arr1) = DataFrame d (f arr1)
-- {-# INLINE mapV #-}

unsafeProof :: p a -> q b -> a :~: b
unsafeProof _ _ = unsafeCoerce Refl
{-# INLINE unsafeProof #-}

unsafeIsFixed :: p xns -> q ns
             -> (IsFixedDim xns ns) :~: 'True
unsafeIsFixed _ _ = unsafeCoerce Refl
{-# INLINE unsafeIsFixed #-}
