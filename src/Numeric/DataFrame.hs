{-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
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

import           Control.Arrow      (first, second)
import           Data.Type.Equality
import           GHC.TypeLits       (Nat)
import           GHC.Types          (Type)
import           Numeric.Array
import           Numeric.Commons
import           Numeric.Dimensions
import           Text.Read
import           Unsafe.Coerce

-- | Keep data in a primitive data frame
--    and maintain information about dimensions in the type-system
data DataFrame :: (Type -> [k] -> Type) where
   DataFrameKnown :: forall t ns
                   . Dimensions (NatList ns)
                  => Array t (NatList ns) -> DataFrame t ns
   DataFrameSome  :: forall t ns xns
                   . ( Dimensions ns
                     , FixedDim xns ns ~ ns
                     )
                  => Dim xns -> Array t (FixedDim xns ns) -> DataFrame t xns


type family NatList (x::[k]) :: [Nat] where
  NatList xs = xs


wrapKnown :: Dimensions ns => Array t ns -> DataFrame t ns
wrapKnown a = case f a of
  Refl -> DataFrameKnown a
  where
    f :: a ns -> ns :~: NatList ns
    f _ = unsafeCoerce Refl

instance ( Show (Array t (NatList ds))
         ) => Show (DataFrame t (ds :: [Nat])) where
  show (DataFrameKnown arr) = "DataFrame:"
                         ++ "\n\tShape: " ++ show (dim `inSpaceOf` arr)
                         ++ "\n\tContent:\n" ++ show arr
  show DataFrameSome {} = undefined

dData :: NatList ds ~ ds => DataFrame t ds -> Array t (ds :: [Nat])
dData (DataFrameKnown a)  = a
dData (DataFrameSome _ a) = unsafeCoerce a



instance PreservingDim (DataFrame t) (DataFrame t) where
  shape x@(DataFrameKnown a) = case unsafeProof x a of
    Refl -> dim
  shape (DataFrameSome ds _) = ds
  looseDims (DataFrameKnown a)
      = let rez = DataFrameSome d a
            d = xdim (dim `inSpaceOf` a) `inSpaceOf` rez
        in rez
  looseDims (DataFrameSome _ _) = error
    "Something is wrong: DataFrameSome should be parameterized by XNat."
  withShape (DataFrameSome _ a) f = f (wrapKnown a)
  withShape (DataFrameKnown _) _ = error
    "Something is wrong: DataFrameKnown should be parameterized by Nat."


instance ( Bounded (Array t ds)
         , Dimensions ds
         , NatList ds ~ ds
         ) => Bounded (DataFrame t ds) where
  minBound = DataFrameKnown minBound
  {-# INLINE minBound #-}
  maxBound = DataFrameKnown maxBound
  {-# INLINE maxBound #-}

instance ( Enum (Array t ds)
         , Dimensions ds
         , NatList ds ~ ds
         ) => Enum (DataFrame t ds) where
  succ = mapV succ
  {-# INLINE succ #-}
  pred = mapV pred
  {-# INLINE pred #-}
  toEnum = DataFrameKnown . toEnum
  {-# INLINE toEnum #-}
  fromEnum = fromEnum . dData
  {-# INLINE fromEnum #-}
  enumFrom = fmap DataFrameKnown . enumFrom . dData
  {-# INLINE enumFrom #-}
  enumFromTo x y
    = DataFrameKnown <$> enumFromTo (dData x) (dData y)
  {-# INLINE enumFromTo #-}
  enumFromThen x x'
    = DataFrameKnown <$> enumFromThen (dData x) (dData x')
  {-# INLINE enumFromThen #-}
  enumFromThenTo x x' y
    = DataFrameKnown <$> enumFromThenTo (dData x) (dData x') (dData y)
  {-# INLINE enumFromThenTo #-}

instance ( Eq (Array t (NatList ds))
         , NatList ds ~ ds
         ) => Eq (DataFrame t ds) where
  DataFrameKnown arr1 == DataFrameKnown arr2 = arr1 == arr2
  _ == _ = undefined
  {-# INLINE (==) #-}
  DataFrameKnown arr1 /= DataFrameKnown arr2 = arr1 /= arr2
  _ /= _ = undefined
  {-# INLINE (/=) #-}

instance ( Integral (Array t ds)
         , Real (DataFrame t ds)
         , Enum (DataFrame t ds)
         , NatList ds ~ ds
         ) => Integral (DataFrame t ds) where
  quot = zipV quot
  {-# INLINE quot #-}
  rem = zipV rem
  {-# INLINE rem #-}
  div = zipV div
  {-# INLINE div #-}
  mod = zipV mod
  {-# INLINE mod #-}
  quotRem (DataFrameKnown x) (DataFrameKnown y) = case quotRem x y of
    (a,b) -> (DataFrameKnown a, DataFrameKnown b)
  quotRem _ _ = undefined
  {-# INLINE quotRem #-}
  divMod (DataFrameKnown x) (DataFrameKnown y) = case divMod x y of
    (a,b) -> (DataFrameKnown a, DataFrameKnown b)
  divMod _ _ = undefined
  {-# INLINE divMod #-}
  toInteger = toInteger . dData
  {-# INLINE toInteger #-}

-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--     and lexicographical ordering for `compare`
instance ( Ord (Array t ds)
         , NatList ds ~ ds
         )  => Ord (DataFrame t ds) where
  (>) = combineV (>)
  {-# INLINE (>) #-}
  (<) = combineV (<)
  {-# INLINE (<) #-}
  (>=) = combineV (>=)
  {-# INLINE (>=) #-}
  (<=) = combineV (<=)
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare = combineV compare
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min = zipV min
  {-# INLINE min #-}
  -- | Element-wise maximum
  max = zipV max
  {-# INLINE max #-}


instance ( Num (Array t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => Num (DataFrame t ds) where
  (+) = zipV (+)
  {-# INLINE (+) #-}
  (-) = zipV (-)
  {-# INLINE (-) #-}
  (*) = zipV (*)
  {-# INLINE (*) #-}
  negate = mapV negate
  {-# INLINE negate #-}
  abs = mapV abs
  {-# INLINE abs #-}
  signum = mapV signum
  {-# INLINE signum #-}
  fromInteger = DataFrameKnown . fromInteger
  {-# INLINE fromInteger #-}

instance ( Fractional (Array t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => Fractional (DataFrame t ds) where
  (/) = zipV (/)
  {-# INLINE (/) #-}
  recip = mapV recip
  {-# INLINE recip #-}
  fromRational = DataFrameKnown . fromRational
  {-# INLINE fromRational #-}


instance ( Floating (Array t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => Floating (DataFrame t ds) where
  pi = DataFrameKnown pi
  {-# INLINE pi #-}
  exp = mapV exp
  {-# INLINE exp #-}
  log = mapV log
  {-# INLINE log #-}
  sqrt = mapV sqrt
  {-# INLINE sqrt #-}
  sin = mapV sin
  {-# INLINE sin #-}
  cos = mapV cos
  {-# INLINE cos #-}
  tan = mapV tan
  {-# INLINE tan #-}
  asin = mapV asin
  {-# INLINE asin #-}
  acos = mapV acos
  {-# INLINE acos #-}
  atan = mapV atan
  {-# INLINE atan #-}
  sinh = mapV sinh
  {-# INLINE sinh #-}
  cosh = mapV cosh
  {-# INLINE cosh #-}
  tanh = mapV tanh
  {-# INLINE tanh #-}
  (**) = zipV (**)
  {-# INLINE (**) #-}
  logBase = zipV logBase
  {-# INLINE logBase #-}
  asinh = mapV asinh
  {-# INLINE asinh #-}
  acosh = mapV acosh
  {-# INLINE acosh #-}
  atanh = mapV atanh
  {-# INLINE atanh #-}

instance ( Read (Array t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => Read (DataFrame t ds) where
  readsPrec n =  map (first DataFrameKnown) . readsPrec n
  {-# INLINE readsPrec #-}
  readList = map (first (map DataFrameKnown)) . readList
  {-# INLINE readList #-}
  readPrec = DataFrameKnown <$> readPrec
  {-# INLINE readPrec #-}
  readListPrec = map DataFrameKnown <$> readListPrec
  {-# INLINE readListPrec #-}

instance ( Real (Array t ds)
         , Num (DataFrame t ds)
         , NatList ds ~ ds
         ) => Real (DataFrame t ds) where
  toRational = toRational . dData
  {-# INLINE toRational #-}


instance ( RealFrac (Array t ds)
         , Real (DataFrame t ds)
         , Fractional (DataFrame t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => RealFrac (DataFrame t ds) where
  properFraction = second DataFrameKnown . properFraction . dData
  {-# INLINE properFraction #-}
  truncate = truncate . dData
  {-# INLINE truncate #-}
  round = round . dData
  {-# INLINE round #-}
  ceiling = ceiling . dData
  {-# INLINE ceiling #-}
  floor = floor . dData
  {-# INLINE floor #-}

instance ( RealFloat (Array t ds)
         , RealFrac (DataFrame t ds)
         , Floating (DataFrame t ds)
         , Dimensions ds
         , NatList ds ~ ds
         ) => RealFloat (DataFrame t ds) where
   floatRadix = floatRadix . dData
   {-# INLINE floatRadix #-}
   floatDigits = floatDigits . dData
   {-# INLINE floatDigits #-}
   floatRange = floatRange . dData
   {-# INLINE floatRange #-}
   decodeFloat = decodeFloat . dData
   {-# INLINE decodeFloat #-}
   encodeFloat i = DataFrameKnown . encodeFloat i
   {-# INLINE encodeFloat #-}
   exponent = exponent . dData
   {-# INLINE exponent #-}
   significand = mapV significand
   {-# INLINE significand #-}
   scaleFloat i = mapV (scaleFloat i)
   {-# INLINE scaleFloat #-}
   isNaN = isNaN  . dData
   {-# INLINE isNaN #-}
   isInfinite = isInfinite . dData
   {-# INLINE isInfinite #-}
   isDenormalized = isDenormalized . dData
   {-# INLINE isDenormalized #-}
   isNegativeZero = isNegativeZero . dData
   {-# INLINE isNegativeZero #-}
   isIEEE = isIEEE . dData
   {-# INLINE isIEEE #-}
   atan2 = zipV atan2
   {-# INLINE atan2 #-}


instance ( PrimBytes (Array t ds)
         , Dimensions ds
         , NatList ds ~ ds
         ) => PrimBytes (DataFrame t ds) where
  toBytes x = toBytes (dData x)
  {-# INLINE toBytes #-}
  fromBytes b = DataFrameKnown (fromBytes b)
  {-# INLINE fromBytes #-}
  byteSize x = byteSize (dData x)
  {-# INLINE byteSize #-}
  byteAlign x = byteAlign (dData x)
  {-# INLINE byteAlign #-}
  elementByteSize x = elementByteSize (dData x)
  {-# INLINE elementByteSize #-}

instance ( FloatBytes (Array t ds)
         , NatList ds ~ ds
         ) => FloatBytes (DataFrame t ds) where
  ixF i x = ixF i (dData x)
instance ( DoubleBytes (Array t ds)
         , NatList ds ~ ds
         ) => DoubleBytes (DataFrame t ds) where
  ixD i x = ixD i (dData x)
instance ( IntBytes (Array t ds)
         , NatList ds ~ ds
         ) => IntBytes (DataFrame t ds) where
  ixI i x = ixI i (dData x)
instance ( WordBytes (Array t ds)
         , NatList ds ~ ds
         ) => WordBytes (DataFrame t ds) where
  ixW i x = ixW i (dData x)


instance ( ElementWise (Idx ds) t (Array t ds)
         , NatList ds ~ ds
         , Dimensions ds
         ) => ElementWise (Idx ds) t (DataFrame t ds) where
  (!) = (!) . dData
  {-# INLINE (!) #-}
  ewmap f = DataFrameKnown . ewmap f . dData
  {-# INLINE ewmap #-}
  ewgen = DataFrameKnown . ewgen
  {-# INLINE ewgen #-}
  ewfold f x0 = ewfold f x0 . dData
  {-# INLINE ewfold #-}
  elementWise f = fmap DataFrameKnown . elementWise f . dData
  {-# INLINE elementWise #-}
  indexWise f = fmap DataFrameKnown . indexWise f . dData
  {-# INLINE indexWise #-}



combineV :: (Array t ds -> Array t ds -> a)
         -> DataFrame t ds -> DataFrame t ds -> a
combineV f x@(DataFrameKnown arr1) (DataFrameKnown arr2)
  = case unsafeIsNatList x of Refl -> f arr1 arr2
combineV _ _ _ = undefined
{-# INLINE combineV #-}

zipV :: (Array t ds -> Array t ds -> Array t ds)
     -> DataFrame t ds -> DataFrame t ds -> DataFrame t ds
zipV f x@(DataFrameKnown arr1) (DataFrameKnown arr2)
  = case unsafeIsNatList x of Refl -> DataFrameKnown (f arr1 arr2)
zipV _ _ _ = undefined
{-# INLINE zipV #-}

mapV :: (Array t (NatList ds) -> Array t (NatList ds))
     -> DataFrame t ds -> DataFrame t ds
mapV f (DataFrameKnown arr1) = DataFrameKnown (f arr1)
mapV _ _                     = undefined
{-# INLINE mapV #-}

unsafeIsNatList :: t ds -> NatList ds :~: ds
unsafeIsNatList _ = unsafeCoerce Refl

unsafeProof :: p a -> q b -> a :~: b
unsafeProof _ _ = unsafeCoerce Refl
{-# INLINE unsafeProof #-}
