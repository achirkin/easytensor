{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.DataFrame.Internal.Array.Family.DoubleX2 (DoubleX2 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import {-# SOURCE #-} Numeric.DataFrame.Internal.Array.Family  (Array)
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.PrimBytes


data DoubleX2 = DoubleX2# Double# Double#

#define IS_ARRAY Array Double '[2] ~ DoubleX2

instance {-# OVERLAPPING #-} Bounded (DataFrame Double '[2]) where
    maxBound = case inftyD of D# x -> unsafeCoerce# (DoubleX2# x x)
    {-# INLINE maxBound #-}
    minBound = case negate inftyD of D# x -> unsafeCoerce# (DoubleX2# x x)
    {-# INLINE minBound #-}


-- instance Show (DataFrame Double '[2]) where
--     show (SingleFrame (DoubleX2# a1 a2))
--       =  "{ " ++ show (D# a1)
--       ++ ", " ++ show (D# a2)
--       ++ " }"



instance {-# OVERLAPPING #-} IS_ARRAY => Eq (DataFrame Double '[2]) where

    SingleFrame (DoubleX2# a1 a2) == SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (       (a1 ==## b1)
      `andI#` (a2 ==## b2)
      )
    {-# INLINE (==) #-}

    SingleFrame (DoubleX2# a1 a2) /= SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (      (a1 /=## b1)
      `orI#` (a2 /=## b2)
      )
    {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance {-# OVERLAPPING #-} IS_ARRAY => Ord (DataFrame Double '[2]) where
    SingleFrame (DoubleX2# a1 a2) > SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (       (a1 >## b1)
      `andI#` (a2 >## b2)
      )
    {-# INLINE (>) #-}

    SingleFrame (DoubleX2# a1 a2) < SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (       (a1 <## b1)
      `andI#` (a2 <## b2)
      )
    {-# INLINE (<) #-}

    SingleFrame (DoubleX2# a1 a2) >= SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (       (a1 >=## b1)
      `andI#` (a2 >=## b2)
      )
    {-# INLINE (>=) #-}

    SingleFrame (DoubleX2# a1 a2) <= SingleFrame (DoubleX2# b1 b2) =
      isTrue#
      (       (a1 <=## b1)
      `andI#` (a2 <=## b2)
      )
    {-# INLINE (<=) #-}

    -- | Compare lexicographically
    compare (SingleFrame (DoubleX2# a1 a2)) (SingleFrame (DoubleX2# b1 b2))
      | isTrue# (a1 >## b1) = GT
      | isTrue# (a1 <## b1) = LT
      | isTrue# (a2 >## b2) = GT
      | isTrue# (a2 <## b2) = LT
      | otherwise = EQ
    {-# INLINE compare #-}

    -- | Element-wise minimum
    min (SingleFrame (DoubleX2# a1 a2)) (SingleFrame (DoubleX2# b1 b2)) = unsafeCoerce# (DoubleX2#
      (if isTrue# (a1 >## b1) then b1 else a1))
      (if isTrue# (a2 >## b2) then b2 else a2)
    {-# INLINE min #-}

    -- | Element-wise maximum
    max (SingleFrame (DoubleX2# a1 a2)) (SingleFrame (DoubleX2# b1 b2)) = unsafeCoerce# (DoubleX2#
      (if isTrue# (a1 >## b1) then a1 else b1))
      (if isTrue# (a2 >## b2) then a2 else b2)
    {-# INLINE max #-}



-- | element-wise operations for vectors
instance {-# OVERLAPPING #-} IS_ARRAY => Num (DataFrame Double '[2]) where

    SingleFrame (DoubleX2# a1 a2) + SingleFrame (DoubleX2# b1 b2)
      = unsafeCoerce# (DoubleX2# ((+##) a1 b1) ((+##) a2 b2))
    {-# INLINE (+) #-}

    SingleFrame (DoubleX2# a1 a2) - SingleFrame (DoubleX2# b1 b2)
      = unsafeCoerce# (DoubleX2# ((-##) a1 b1) ((-##) a2 b2))
    {-# INLINE (-) #-}

    SingleFrame (DoubleX2# a1 a2) * SingleFrame (DoubleX2# b1 b2)
      = unsafeCoerce# (DoubleX2# ((*##) a1 b1) ((*##) a2 b2))
    {-# INLINE (*) #-}

    negate (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (negateDouble# a1) (negateDouble# a2))
    {-# INLINE negate #-}

    abs (SingleFrame (DoubleX2# a1 a2))
      = unsafeCoerce# (DoubleX2#
      (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1))
      (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
    {-# INLINE abs #-}

    signum (SingleFrame (DoubleX2# a1 a2))
      = unsafeCoerce# (DoubleX2# (if isTrue# (a1 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a2 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## ))
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of D# x -> SingleFrame (DoubleX2# x x)
    {-# INLINE fromInteger #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Fractional (DataFrame Double '[2]) where

    SingleFrame (DoubleX2# a1 a2) / SingleFrame (DoubleX2# b1 b2) = unsafeCoerce# (DoubleX2#
      ((/##) a1 b1) ((/##) a2 b2))
    {-# INLINE (/) #-}

    recip (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      ((/##) 1.0## a1) ((/##) 1.0## a2))
    {-# INLINE recip #-}

    fromRational r = case fromRational r of D# x -> SingleFrame (DoubleX2# x x)
    {-# INLINE fromRational #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Floating (DataFrame Double '[2]) where

    pi = unsafeCoerce# (DoubleX2#
      3.141592653589793238##)
      3.141592653589793238##
    {-# INLINE pi #-}

    exp (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (expDouble# a1) (expDouble# a2))
    {-# INLINE exp #-}

    log (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (logDouble# a1) (logDouble# a2))
    {-# INLINE log #-}

    sqrt (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (sqrtDouble# a1) (sqrtDouble# a2))
    {-# INLINE sqrt #-}

    sin (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (sinDouble# a1) (sinDouble# a2))
    {-# INLINE sin #-}

    cos (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (cosDouble# a1) (cosDouble# a2))
    {-# INLINE cos #-}

    tan (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (tanDouble# a1) (tanDouble# a2))
    {-# INLINE tan #-}

    asin (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (asinDouble# a1) (asinDouble# a2))
    {-# INLINE asin #-}

    acos (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (acosDouble# a1) (acosDouble# a2))
    {-# INLINE acos #-}

    atan (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (atanDouble# a1) (atanDouble# a2))
    {-# INLINE atan #-}

    sinh (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (sinhDouble# a1) (sinhDouble# a2))
    {-# INLINE sinh #-}

    cosh (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (coshDouble# a1) (coshDouble# a2))
    {-# INLINE cosh #-}

    tanh (SingleFrame (DoubleX2# a1 a2)) = unsafeCoerce# (DoubleX2#
      (tanhDouble# a1) (tanhDouble# a2))
    {-# INLINE tanh #-}

    SingleFrame (DoubleX2# a1 a2) ** SingleFrame (DoubleX2# b1 b2) = unsafeCoerce# (DoubleX2#
      ((**##) a1 b1) ((**##) a2 b2))
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
#define BOFF_TO_PRIMOFF(off) uncheckedIShiftRL# off 3#
#define ELEM_N 2

instance {-# OVERLAPPING #-} IS_ARRAY => PrimBytes (DataFrame Double '[2]) where

    getBytes (SingleFrame (DoubleX2# a1 a2)) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @(DataFrame Double '[2]) undefined) s0 of
           (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
             s2 -> case writeDoubleArray# marr 1# a2 s2 of
               s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = unsafeCoerce# (DoubleX2#
      (indexDoubleArray# arr i))
      (indexDoubleArray# arr (i +# 1#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (DoubleX2# a1 a2) #)
    {-# INLINE readBytes #-}

    writeBytes mba off (SingleFrame (DoubleX2# a1 a2)) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readDoubleOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readDoubleOffAddr# addr 1# s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (DoubleX2# a1 a2) #)
    {-# INLINE readAddr #-}

    writeAddr (SingleFrame (DoubleX2# a1 a2)) addr s
      = writeDoubleOffAddr# addr 1# a2
      ( writeDoubleOffAddr# addr 0# a1 s )
    {-# INLINE writeAddr #-}

    byteSize _ = byteSize @Double undefined *# ELEM_N#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Double undefined
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    indexArray ba off
      | i <- off *# ELEM_N#
      = unsafeCoerce# (DoubleX2#
      (indexDoubleArray# ba i))
      (indexDoubleArray# ba (i +# 1#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (DoubleX2# a1 a2) #)
    {-# INLINE readArray #-}

    writeArray mba off (SingleFrame (DoubleX2# a1 a2)) s
      | i <- off *# ELEM_N#
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeArray #-}


instance {-# OVERLAPPING #-} IS_ARRAY => PrimArray Double (DataFrame Double '[2]) where

    broadcast (D# x) = SingleFrame (DoubleX2# x x)
    {-# INLINE broadcast #-}

    ix# 0# (SingleFrame (DoubleX2# a1 _)) = D# a1
    ix# 1# (SingleFrame (DoubleX2# _ a2)) = D# a2
    ix# _   _               = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, D# a1 #) -> case f s1 of
        (# s2, D# a2 #) -> (# s2, SingleFrame (DoubleX2# a1 a2) #)

    upd# _ 0# (D# q) (SingleFrame (DoubleX2# _ y)) = SingleFrame (DoubleX2# q y)
    upd# _ 1# (D# q) (SingleFrame (DoubleX2# x _)) = SingleFrame (DoubleX2# x q)
    upd# _ _ _ x                     = x
    {-# INLINE upd# #-}

    elemOffset _ = 0#
    {-# INLINE elemOffset #-}

    elemSize0 _  = ELEM_N#
    {-# INLINE elemSize0 #-}

    fromElems off _ ba = unsafeCoerce# (DoubleX2#
      (indexDoubleArray# ba off))
      (indexDoubleArray# ba (off +# 1#))
    {-# INLINE fromElems #-}
