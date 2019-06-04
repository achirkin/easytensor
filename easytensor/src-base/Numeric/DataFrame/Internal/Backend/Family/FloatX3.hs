{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedSums           #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.DataFrame.Internal.Backend.Family.FloatX3 (FloatX3 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial


data FloatX3 = FloatX3# Float# Float# Float#

-- | Since @Bounded@ is not implemented for floating point types, this instance
--   has an unresolvable constraint.
--   Nevetheless, it is good to have it here for nicer error messages.
instance Bounded Float => Bounded FloatX3 where
    maxBound = case maxBound of F# x -> FloatX3# x x x
    minBound = case minBound of F# x -> FloatX3# x x x


instance Eq FloatX3 where

    FloatX3# a1 a2 a3 == FloatX3# b1 b2 b3 =
      isTrue#
      (       (a1 `eqFloat#` b1)
      `andI#` (a2 `eqFloat#` b2)
      `andI#` (a3 `eqFloat#` b3)
      )
    {-# INLINE (==) #-}

    FloatX3# a1 a2 a3 /= FloatX3# b1 b2 b3 =
      isTrue#
      (      (a1 `neFloat#` b1)
      `orI#` (a2 `neFloat#` b2)
      `orI#` (a3 `neFloat#` b3)
      )
    {-# INLINE (/=) #-}




cmp' :: Float# -> Float# -> PartialOrdering
cmp' a b
  | isTrue# (a `gtFloat#` b) = PGT
  | isTrue# (a `ltFloat#` b) = PLT
  | otherwise  = PEQ

instance ProductOrder FloatX3 where
    cmp (FloatX3# a1 a2 a3) (FloatX3# b1 b2 b3)
      = cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
    {-# INLINE cmp #-}

instance Ord (NonTransitive.ProductOrd FloatX3) where
    NonTransitive.ProductOrd x > NonTransitive.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    NonTransitive.ProductOrd x < NonTransitive.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (NonTransitive.ProductOrd (FloatX3# a1 a2 a3))
         (NonTransitive.ProductOrd (FloatX3# b1 b2 b3)) = isTrue#
      ((a1 `geFloat#` b1) `andI#` (a2 `geFloat#` b2) `andI#` (a3 `geFloat#` b3))
    {-# INLINE (>=) #-}
    (<=) (NonTransitive.ProductOrd (FloatX3# a1 a2 a3))
         (NonTransitive.ProductOrd (FloatX3# b1 b2 b3)) = isTrue#
      ((a1 `leFloat#` b1) `andI#` (a2 `leFloat#` b2) `andI#` (a3 `leFloat#` b3))
    {-# INLINE (<=) #-}
    compare (NonTransitive.ProductOrd a) (NonTransitive.ProductOrd b)
      = NonTransitive.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (NonTransitive.ProductOrd (FloatX3# a1 a2 a3))
        (NonTransitive.ProductOrd (FloatX3# b1 b2 b3))
      = NonTransitive.ProductOrd
        ( FloatX3#
          (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
          (if isTrue# (a3 `gtFloat#` b3) then b3 else a3)
        )
    {-# INLINE min #-}
    max (NonTransitive.ProductOrd (FloatX3# a1 a2 a3))
        (NonTransitive.ProductOrd (FloatX3# b1 b2 b3))
      = NonTransitive.ProductOrd
        ( FloatX3#
          (if isTrue# (a1 `ltFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `ltFloat#` b2) then b2 else a2)
          (if isTrue# (a3 `ltFloat#` b3) then b3 else a3)
        )
    {-# INLINE max #-}

instance Ord (Partial.ProductOrd FloatX3) where
    Partial.ProductOrd x > Partial.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    Partial.ProductOrd x < Partial.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (Partial.ProductOrd (FloatX3# a1 a2 a3))
         (Partial.ProductOrd (FloatX3# b1 b2 b3)) = isTrue#
      ((a1 `geFloat#` b1) `andI#` (a2 `geFloat#` b2) `andI#` (a3 `geFloat#` b3))
    {-# INLINE (>=) #-}
    (<=) (Partial.ProductOrd (FloatX3# a1 a2 a3))
         (Partial.ProductOrd (FloatX3# b1 b2 b3)) = isTrue#
      ((a1 `leFloat#` b1) `andI#` (a2 `leFloat#` b2) `andI#` (a3 `leFloat#` b3))
    {-# INLINE (<=) #-}
    compare (Partial.ProductOrd a) (Partial.ProductOrd b)
      = Partial.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (Partial.ProductOrd (FloatX3# a1 a2 a3))
        (Partial.ProductOrd (FloatX3# b1 b2 b3))
      = Partial.ProductOrd
        ( FloatX3#
          (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
          (if isTrue# (a3 `gtFloat#` b3) then b3 else a3)
        )
    {-# INLINE min #-}
    max (Partial.ProductOrd (FloatX3# a1 a2 a3))
        (Partial.ProductOrd (FloatX3# b1 b2 b3))
      = Partial.ProductOrd
        ( FloatX3#
          (if isTrue# (a1 `ltFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `ltFloat#` b2) then b2 else a2)
          (if isTrue# (a3 `ltFloat#` b3) then b3 else a3)
        )
    {-# INLINE max #-}

instance Ord FloatX3 where
    FloatX3# a1 a2 a3 > FloatX3# b1 b2 b3
      | isTrue# (a1 `gtFloat#` b1) = True
      | isTrue# (a1 `ltFloat#` b1) = False
      | isTrue# (a2 `gtFloat#` b2) = True
      | isTrue# (a2 `ltFloat#` b2) = False
      | isTrue# (a3 `gtFloat#` b3) = True
      | otherwise           = False
    {-# INLINE (>) #-}

    FloatX3# a1 a2 a3 < FloatX3# b1 b2 b3
      | isTrue# (a1 `ltFloat#` b1) = True
      | isTrue# (a1 `gtFloat#` b1) = False
      | isTrue# (a2 `ltFloat#` b2) = True
      | isTrue# (a2 `gtFloat#` b2) = False
      | isTrue# (a3 `ltFloat#` b3) = True
      | otherwise           = False
    {-# INLINE (<) #-}

    FloatX3# a1 a2 a3 >= FloatX3# b1 b2 b3
      | isTrue# (a1 `ltFloat#` b1) = False
      | isTrue# (a1 `gtFloat#` b1) = True
      | isTrue# (a2 `ltFloat#` b2) = False
      | isTrue# (a2 `gtFloat#` b2) = True
      | isTrue# (a3 `ltFloat#` b3) = False
      | otherwise           = True
    {-# INLINE (>=) #-}

    FloatX3# a1 a2 a3 <= FloatX3# b1 b2 b3
      | isTrue# (a1 `gtFloat#` b1) = False
      | isTrue# (a1 `ltFloat#` b1) = True
      | isTrue# (a2 `gtFloat#` b2) = False
      | isTrue# (a2 `ltFloat#` b2) = True
      | isTrue# (a3 `gtFloat#` b3) = False
      | otherwise           = True
    {-# INLINE (<=) #-}

    compare (FloatX3# a1 a2 a3) (FloatX3# b1 b2 b3)
      | isTrue# (a1 `gtFloat#` b1) = GT
      | isTrue# (a1 `ltFloat#` b1) = LT
      | isTrue# (a2 `gtFloat#` b2) = GT
      | isTrue# (a2 `ltFloat#` b2) = LT
      | isTrue# (a3 `gtFloat#` b3) = GT
      | isTrue# (a3 `ltFloat#` b3) = LT
      | otherwise           = EQ
    {-# INLINE compare #-}




-- | element-wise operations for vectors
instance Num FloatX3 where

    FloatX3# a1 a2 a3 + FloatX3# b1 b2 b3
      = FloatX3# (plusFloat# a1 b1) (plusFloat# a2 b2) (plusFloat# a3 b3)
    {-# INLINE (+) #-}

    FloatX3# a1 a2 a3 - FloatX3# b1 b2 b3
      = FloatX3# (minusFloat# a1 b1) (minusFloat# a2 b2) (minusFloat# a3 b3)
    {-# INLINE (-) #-}

    FloatX3# a1 a2 a3 * FloatX3# b1 b2 b3
      = FloatX3# (timesFloat# a1 b1) (timesFloat# a2 b2) (timesFloat# a3 b3)
    {-# INLINE (*) #-}

    negate (FloatX3# a1 a2 a3) = FloatX3#
      (negateFloat# a1) (negateFloat# a2) (negateFloat# a3)
    {-# INLINE negate #-}

    abs (FloatX3# a1 a2 a3)
      = FloatX3#
      (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1)
      (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
      (if isTrue# (a3 `geFloat#` 0.0#) then a3 else negateFloat# a3)
    {-# INLINE abs #-}

    signum (FloatX3# a1 a2 a3)
      = FloatX3# (if isTrue# (a1 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                 (if isTrue# (a2 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a2 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                 (if isTrue# (a3 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a3 `ltFloat#` 0.0#) then -1.0# else 0.0# )
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of F# x -> FloatX3# x x x
    {-# INLINE fromInteger #-}



instance Fractional FloatX3 where

    FloatX3# a1 a2 a3 / FloatX3# b1 b2 b3 = FloatX3#
      (divideFloat# a1 b1) (divideFloat# a2 b2) (divideFloat# a3 b3)
    {-# INLINE (/) #-}

    recip (FloatX3# a1 a2 a3) = FloatX3#
      (divideFloat# 1.0# a1) (divideFloat# 1.0# a2) (divideFloat# 1.0# a3)
    {-# INLINE recip #-}

    fromRational r = case fromRational r of F# x -> FloatX3# x x x
    {-# INLINE fromRational #-}



instance Floating FloatX3 where

    pi = FloatX3#
      3.141592653589793238#
      3.141592653589793238#
      3.141592653589793238#
    {-# INLINE pi #-}

    exp (FloatX3# a1 a2 a3) = FloatX3#
      (expFloat# a1) (expFloat# a2) (expFloat# a3)
    {-# INLINE exp #-}

    log (FloatX3# a1 a2 a3) = FloatX3#
      (logFloat# a1) (logFloat# a2) (logFloat# a3)
    {-# INLINE log #-}

    sqrt (FloatX3# a1 a2 a3) = FloatX3#
      (sqrtFloat# a1) (sqrtFloat# a2) (sqrtFloat# a3)
    {-# INLINE sqrt #-}

    sin (FloatX3# a1 a2 a3) = FloatX3#
      (sinFloat# a1) (sinFloat# a2) (sinFloat# a3)
    {-# INLINE sin #-}

    cos (FloatX3# a1 a2 a3) = FloatX3#
      (cosFloat# a1) (cosFloat# a2) (cosFloat# a3)
    {-# INLINE cos #-}

    tan (FloatX3# a1 a2 a3) = FloatX3#
      (tanFloat# a1) (tanFloat# a2) (tanFloat# a3)
    {-# INLINE tan #-}

    asin (FloatX3# a1 a2 a3) = FloatX3#
      (asinFloat# a1) (asinFloat# a2) (asinFloat# a3)
    {-# INLINE asin #-}

    acos (FloatX3# a1 a2 a3) = FloatX3#
      (acosFloat# a1) (acosFloat# a2) (acosFloat# a3)
    {-# INLINE acos #-}

    atan (FloatX3# a1 a2 a3) = FloatX3#
      (atanFloat# a1) (atanFloat# a2) (atanFloat# a3)
    {-# INLINE atan #-}

    sinh (FloatX3# a1 a2 a3) = FloatX3#
      (sinhFloat# a1) (sinhFloat# a2) (sinhFloat# a3)
    {-# INLINE sinh #-}

    cosh (FloatX3# a1 a2 a3) = FloatX3#
      (coshFloat# a1) (coshFloat# a2) (coshFloat# a3)
    {-# INLINE cosh #-}

    tanh (FloatX3# a1 a2 a3) = FloatX3#
      (tanhFloat# a1) (tanhFloat# a2) (tanhFloat# a3)
    {-# INLINE tanh #-}

    FloatX3# a1 a2 a3 ** FloatX3# b1 b2 b3 = FloatX3#
      (powerFloat# a1 b1) (powerFloat# a2 b2) (powerFloat# a3 b3)
    {-# INLINE (**) #-}

    logBase x y         =  log y / log x
    {-# INLINE logBase #-}

    asinh x = log (x + sqrt (1.0+x*x))
    {-# INLINE asinh #-}

    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    {-# INLINE acosh #-}

    atanh x = 0.5 * log ((1.0+x) / (1.0-x))
    {-# INLINE atanh #-}

-- offset in bytes is S times bigger than offset in prim elements,
-- when S is power of two, this is equal to shift
#define BOFF_TO_PRIMOFF(off) uncheckedIShiftRL# off 2#
#define ELEM_N 3

instance PrimBytes FloatX3 where

    getBytes (FloatX3# a1 a2 a3) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @FloatX3 undefined) s0 of
           (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
             s2 -> case writeFloatArray# marr 1# a2 s2 of
               s3 -> case writeFloatArray# marr 2# a3 s3 of
                 s4 -> unsafeFreezeByteArray# marr s4
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = FloatX3#
      (indexFloatArray# arr i)
      (indexFloatArray# arr (i +# 1#))
      (indexFloatArray# arr (i +# 2#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readFloatArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> (# s3, FloatX3# a1 a2 a3 #)
    {-# INLINE readBytes #-}

    writeBytes mba off (FloatX3# a1 a2 a3) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeFloatArray# mba (i +# 2#) a3
      ( writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s ))
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readFloatOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readFloatOffAddr# addr 1# s1 of
        (# s2, a2 #) -> case readFloatOffAddr# addr 2# s2 of
          (# s3, a3 #) -> (# s3, FloatX3# a1 a2 a3 #)
    {-# INLINE readAddr #-}

    writeAddr (FloatX3# a1 a2 a3) addr s
      = writeFloatOffAddr# addr 2# a3
      ( writeFloatOffAddr# addr 1# a2
      ( writeFloatOffAddr# addr 0# a1 s ))
    {-# INLINE writeAddr #-}

    byteSize _ = byteSize @Float undefined *# ELEM_N#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Float undefined
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    byteFieldOffset _ _ = negateInt# 1#
    {-# INLINE byteFieldOffset #-}

    indexArray ba off
      | i <- off *# ELEM_N#
      = FloatX3#
      (indexFloatArray# ba i)
      (indexFloatArray# ba (i +# 1#))
      (indexFloatArray# ba (i +# 2#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readFloatArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> (# s3, FloatX3# a1 a2 a3 #)
    {-# INLINE readArray #-}

    writeArray mba off (FloatX3# a1 a2 a3) s
      | i <- off *# ELEM_N#
      = writeFloatArray# mba (i +# 2#) a3
      ( writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s ))
    {-# INLINE writeArray #-}


instance PrimArray Float FloatX3 where

    broadcast (F# x) = FloatX3# x x x
    {-# INLINE broadcast #-}

    ix# 0# (FloatX3# a1 _ _) = F# a1
    ix# 1# (FloatX3# _ a2 _) = F# a2
    ix# 2# (FloatX3# _ _ a3) = F# a3
    ix# _   _                = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, F# a1 #) -> case f s1 of
        (# s2, F# a2 #) -> case f s2 of
          (# s3, F# a3 #) -> (# s3, FloatX3# a1 a2 a3 #)


    upd# _ 0# (F# q) (FloatX3# _ y z) = FloatX3# q y z
    upd# _ 1# (F# q) (FloatX3# x _ z) = FloatX3# x q z
    upd# _ 2# (F# q) (FloatX3# x y _) = FloatX3# x y q
    upd# _ _ _ x                      = x
    {-# INLINE upd# #-}

    arrayContent# x = (# | (# CumulDims [ELEM_N, 1], 0#, getBytes x #) #)
    {-# INLINE arrayContent# #-}

    offsetElems _ = 0#
    {-# INLINE offsetElems #-}

    uniqueOrCumulDims _ = Right (CumulDims [ELEM_N, 1])
    {-# INLINE uniqueOrCumulDims #-}

    fromElems _ off ba = FloatX3#
      (indexFloatArray# ba off)
      (indexFloatArray# ba (off +# 1#))
      (indexFloatArray# ba (off +# 2#))
    {-# INLINE fromElems #-}
