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
module Numeric.DataFrame.Internal.Array.Family.FloatX2 (FloatX2 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import {-# SOURCE #-} Numeric.DataFrame.Internal.Array.Family  (Array)
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.PrimBytes


data FloatX2 = FloatX2# Float# Float#

#define IS_ARRAY Array Float '[2] ~ FloatX2

instance {-# OVERLAPPING #-} Bounded (DataFrame Float '[2]) where
    maxBound = case inftyF of F# x -> unsafeCoerce# (FloatX2# x x)
    {-# INLINE maxBound #-}
    minBound = case negate inftyF of F# x -> unsafeCoerce# (FloatX2# x x)
    {-# INLINE minBound #-}


-- instance {-# OVERLAPPING #-} Show (DataFrame Float '[2]) where
--     show (SingleFrame (FloatX2# a1 a2))
--       =  "{ " ++ show (F# a1)
--       ++ ", " ++ show (F# a2)
--       ++ " }"



instance {-# OVERLAPPING #-} IS_ARRAY => Eq (DataFrame Float '[2]) where

    SingleFrame (FloatX2# a1 a2) == SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (       (a1 `eqFloat#` b1)
      `andI#` (a2 `eqFloat#` b2)
      )
    {-# INLINE (==) #-}

    SingleFrame (FloatX2# a1 a2) /= SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (      (a1 `neFloat#` b1)
      `orI#` (a2 `neFloat#` b2)
      )
    {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance {-# OVERLAPPING #-} IS_ARRAY => Ord (DataFrame Float '[2]) where
    SingleFrame (FloatX2# a1 a2) > SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (       (a1 `gtFloat#` b1)
      `andI#` (a2 `gtFloat#` b2)
      )
    {-# INLINE (>) #-}

    SingleFrame (FloatX2# a1 a2) < SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (       (a1 `ltFloat#` b1)
      `andI#` (a2 `ltFloat#` b2)
      )
    {-# INLINE (<) #-}

    SingleFrame (FloatX2# a1 a2) >= SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (       (a1 `geFloat#` b1)
      `andI#` (a2 `geFloat#` b2)
      )
    {-# INLINE (>=) #-}

    SingleFrame (FloatX2# a1 a2) <= SingleFrame (FloatX2# b1 b2) =
      isTrue#
      (       (a1 `leFloat#` b1)
      `andI#` (a2 `leFloat#` b2)
      )
    {-# INLINE (<=) #-}

    -- | Compare lexicographically
    compare (SingleFrame (FloatX2# a1 a2)) (SingleFrame (FloatX2# b1 b2))
      | isTrue# (a1 `gtFloat#` b1) = GT
      | isTrue# (a1 `ltFloat#` b1) = LT
      | isTrue# (a2 `gtFloat#` b2) = GT
      | isTrue# (a2 `ltFloat#` b2) = LT
      | otherwise = EQ
    {-# INLINE compare #-}

    -- | Element-wise minimum
    min (SingleFrame (FloatX2# a1 a2)) (SingleFrame (FloatX2# b1 b2)) = unsafeCoerce# (FloatX2#
      (if isTrue# (a1 `gtFloat#` b1) then b1 else a1))
      (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
    {-# INLINE min #-}

    -- | Element-wise maximum
    max (SingleFrame (FloatX2# a1 a2)) (SingleFrame (FloatX2# b1 b2)) = unsafeCoerce# (FloatX2#
      (if isTrue# (a1 `gtFloat#` b1) then a1 else b1))
      (if isTrue# (a2 `gtFloat#` b2) then a2 else b2)
    {-# INLINE max #-}



-- | element-wise operations for vectors
instance {-# OVERLAPPING #-} IS_ARRAY => Num (DataFrame Float '[2]) where

    SingleFrame (FloatX2# a1 a2) + SingleFrame (FloatX2# b1 b2)
      = unsafeCoerce# (FloatX2# (plusFloat# a1 b1) (plusFloat# a2 b2))
    {-# INLINE (+) #-}

    SingleFrame (FloatX2# a1 a2) - SingleFrame (FloatX2# b1 b2)
      = unsafeCoerce# (FloatX2# (minusFloat# a1 b1) (minusFloat# a2 b2))
    {-# INLINE (-) #-}

    SingleFrame (FloatX2# a1 a2) * SingleFrame (FloatX2# b1 b2)
      = unsafeCoerce# (FloatX2# (timesFloat# a1 b1) (timesFloat# a2 b2))
    {-# INLINE (*) #-}

    negate (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (negateFloat# a1) (negateFloat# a2))
    {-# INLINE negate #-}

    abs (SingleFrame (FloatX2# a1 a2))
      = unsafeCoerce# (FloatX2#
      (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1))
      (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
    {-# INLINE abs #-}

    signum (SingleFrame (FloatX2# a1 a2))
      = unsafeCoerce# (FloatX2# (if isTrue# (a1 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                 (if isTrue# (a2 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a2 `ltFloat#` 0.0#) then -1.0# else 0.0# ))
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of F# x -> SingleFrame (FloatX2# x x)
    {-# INLINE fromInteger #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Fractional (DataFrame Float '[2]) where

    SingleFrame (FloatX2# a1 a2) / SingleFrame (FloatX2# b1 b2) = unsafeCoerce# (FloatX2#
      (divideFloat# a1 b1) (divideFloat# a2 b2))
    {-# INLINE (/) #-}

    recip (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (divideFloat# 1.0# a1) (divideFloat# 1.0# a2))
    {-# INLINE recip #-}

    fromRational r = case fromRational r of F# x -> SingleFrame (FloatX2# x x)
    {-# INLINE fromRational #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Floating (DataFrame Float '[2]) where

    pi = unsafeCoerce# (FloatX2#
      3.141592653589793238#)
      3.141592653589793238#
    {-# INLINE pi #-}

    exp (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (expFloat# a1) (expFloat# a2))
    {-# INLINE exp #-}

    log (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (logFloat# a1) (logFloat# a2))
    {-# INLINE log #-}

    sqrt (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (sqrtFloat# a1) (sqrtFloat# a2))
    {-# INLINE sqrt #-}

    sin (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (sinFloat# a1) (sinFloat# a2))
    {-# INLINE sin #-}

    cos (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (cosFloat# a1) (cosFloat# a2))
    {-# INLINE cos #-}

    tan (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (tanFloat# a1) (tanFloat# a2))
    {-# INLINE tan #-}

    asin (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (asinFloat# a1) (asinFloat# a2))
    {-# INLINE asin #-}

    acos (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (acosFloat# a1) (acosFloat# a2))
    {-# INLINE acos #-}

    atan (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (atanFloat# a1) (atanFloat# a2))
    {-# INLINE atan #-}

    sinh (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (sinhFloat# a1) (sinhFloat# a2))
    {-# INLINE sinh #-}

    cosh (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (coshFloat# a1) (coshFloat# a2))
    {-# INLINE cosh #-}

    tanh (SingleFrame (FloatX2# a1 a2)) = unsafeCoerce# (FloatX2#
      (tanhFloat# a1) (tanhFloat# a2))
    {-# INLINE tanh #-}

    SingleFrame (FloatX2# a1 a2) ** SingleFrame (FloatX2# b1 b2) = unsafeCoerce# (FloatX2#
      (powerFloat# a1 b1) (powerFloat# a2 b2))
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
#define ELEM_N 2

instance {-# OVERLAPPING #-} IS_ARRAY => PrimBytes (DataFrame Float '[2]) where

    getBytes (SingleFrame (FloatX2# a1 a2)) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @(DataFrame Float '[2]) undefined) s0 of
           (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
             s2 -> case writeFloatArray# marr 1# a2 s2 of
               s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = unsafeCoerce# (FloatX2#
      (indexFloatArray# arr i))
      (indexFloatArray# arr (i +# 1#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (FloatX2# a1 a2) #)
    {-# INLINE readBytes #-}

    writeBytes mba off (SingleFrame (FloatX2# a1 a2)) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s )
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readFloatOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readFloatOffAddr# addr 1# s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (FloatX2# a1 a2) #)
    {-# INLINE readAddr #-}

    writeAddr (SingleFrame (FloatX2# a1 a2)) addr s
      = writeFloatOffAddr# addr 1# a2
      ( writeFloatOffAddr# addr 0# a1 s )
    {-# INLINE writeAddr #-}

    byteSize _ = byteSize @Float undefined *# ELEM_N#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Float undefined
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    indexArray ba off
      | i <- off *# ELEM_N#
      = unsafeCoerce# (FloatX2#
      (indexFloatArray# ba i))
      (indexFloatArray# ba (i +# 1#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, SingleFrame (FloatX2# a1 a2) #)
    {-# INLINE readArray #-}

    writeArray mba off (SingleFrame (FloatX2# a1 a2)) s
      | i <- off *# ELEM_N#
      = writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s )
    {-# INLINE writeArray #-}


instance {-# OVERLAPPING #-} IS_ARRAY => PrimArray Float (DataFrame Float '[2]) where

    broadcast (F# x) = SingleFrame (FloatX2# x x)
    {-# INLINE broadcast #-}

    ix# 0# (SingleFrame (FloatX2# a1 _)) = F# a1
    ix# 1# (SingleFrame (FloatX2# _ a2)) = F# a2
    ix# _   _              = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, F# a1 #) -> case f s1 of
        (# s2, F# a2 #) -> (# s2, SingleFrame (FloatX2# a1 a2) #)


    upd# _ 0# (F# q) (SingleFrame (FloatX2# _ y)) = SingleFrame (FloatX2# q y)
    upd# _ 1# (F# q) (SingleFrame (FloatX2# x _)) = SingleFrame (FloatX2# x q)
    upd# _ _ _ x                    = x
    {-# INLINE upd# #-}

    elemOffset _ = 0#
    {-# INLINE elemOffset #-}

    elemSize0 _  = ELEM_N#
    {-# INLINE elemSize0 #-}

    fromElems off _ ba = unsafeCoerce# (FloatX2#
      (indexFloatArray# ba off))
      (indexFloatArray# ba (off +# 1#))
    {-# INLINE fromElems #-}
