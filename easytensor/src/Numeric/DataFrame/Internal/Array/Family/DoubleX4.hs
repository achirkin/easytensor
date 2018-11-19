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
module Numeric.DataFrame.Internal.Array.Family.DoubleX4 (DoubleX4 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import {-# SOURCE #-} Numeric.DataFrame.Internal.Array.Family  (Array)
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.PrimBytes


data DoubleX4 = DoubleX4# Double# Double# Double# Double#

#define IS_ARRAY Array Double '[4] ~ DoubleX4

instance {-# OVERLAPPING #-} Bounded (DataFrame Double '[4]) where
    maxBound = case inftyD of D# x -> unsafeCoerce# (DoubleX4# x x x x)
    {-# INLINE maxBound #-}
    minBound = case negate inftyD of D# x -> unsafeCoerce# (DoubleX4# x x x x)
    {-# INLINE minBound #-}


-- instance {-# OVERLAPPING #-} Show (DataFrame Double '[4]) where
--     show (SingleFrame (DoubleX4# a1 a2 a3 a4))
--       =  "{ " ++ show (D# a1)
--       ++ ", " ++ show (D# a2)
--       ++ ", " ++ show (D# a3)
--       ++ ", " ++ show (D# a4)
--       ++ " }"



instance {-# OVERLAPPING #-} IS_ARRAY => Eq (DataFrame Double '[4]) where

    SingleFrame (DoubleX4# a1 a2 a3 a4) == SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (       (a1 ==## b1)
      `andI#` (a2 ==## b2)
      `andI#` (a3 ==## b3)
      `andI#` (a4 ==## b4)
      )
    {-# INLINE (==) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) /= SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (      (a1 /=## b1)
      `orI#` (a2 /=## b2)
      `orI#` (a3 /=## b3)
      `orI#` (a4 /=## b4)
      )
    {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance {-# OVERLAPPING #-} IS_ARRAY => Ord (DataFrame Double '[4]) where
    SingleFrame (DoubleX4# a1 a2 a3 a4) > SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (       (a1 >## b1)
      `andI#` (a2 >## b2)
      `andI#` (a3 >## b3)
      `andI#` (a4 >## b4)
      )
    {-# INLINE (>) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) < SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (       (a1 <## b1)
      `andI#` (a2 <## b2)
      `andI#` (a3 <## b3)
      `andI#` (a4 <## b4)
      )
    {-# INLINE (<) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) >= SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (       (a1 >=## b1)
      `andI#` (a2 >=## b2)
      `andI#` (a3 >=## b3)
      `andI#` (a4 >=## b4)
      )
    {-# INLINE (>=) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) <= SingleFrame (DoubleX4# b1 b2 b3 b4) =
      isTrue#
      (       (a1 <=## b1)
      `andI#` (a2 <=## b2)
      `andI#` (a3 <=## b3)
      `andI#` (a4 <=## b4)
      )
    {-# INLINE (<=) #-}

    -- | Compare lexicographically
    compare (SingleFrame (DoubleX4# a1 a2 a3 a4)) (SingleFrame (DoubleX4# b1 b2 b3 b4))
      | isTrue# (a1 >## b1) = GT
      | isTrue# (a1 <## b1) = LT
      | isTrue# (a2 >## b2) = GT
      | isTrue# (a2 <## b2) = LT
      | isTrue# (a3 >## b3) = GT
      | isTrue# (a3 <## b3) = LT
      | isTrue# (a4 >## b4) = GT
      | isTrue# (a4 <## b4) = LT
      | otherwise = EQ
    {-# INLINE compare #-}

    -- | Element-wise minimum
    min (SingleFrame (DoubleX4# a1 a2 a3 a4)) (SingleFrame (DoubleX4# b1 b2 b3 b4)) = unsafeCoerce# (DoubleX4#
      (if isTrue# (a1 >## b1) then b1 else a1))
      (if isTrue# (a2 >## b2) then b2 else a2)
      (if isTrue# (a3 >## b3) then b3 else a3)
      (if isTrue# (a4 >## b4) then b4 else a4)
    {-# INLINE min #-}

    -- | Element-wise maximum
    max (SingleFrame (DoubleX4# a1 a2 a3 a4)) (SingleFrame (DoubleX4# b1 b2 b3 b4)) = unsafeCoerce# (DoubleX4#
      (if isTrue# (a1 >## b1) then a1 else b1))
      (if isTrue# (a2 >## b2) then a2 else b2)
      (if isTrue# (a3 >## b3) then a3 else b3)
      (if isTrue# (a4 >## b4) then a4 else b4)
    {-# INLINE max #-}



-- | element-wise operations for vectors
instance {-# OVERLAPPING #-} IS_ARRAY => Num (DataFrame Double '[4]) where

    SingleFrame (DoubleX4# a1 a2 a3 a4) + SingleFrame (DoubleX4# b1 b2 b3 b4)
      = unsafeCoerce# (DoubleX4# ((+##) a1 b1) ((+##) a2 b2) ((+##) a3 b3) ((+##) a4 b4))
    {-# INLINE (+) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) - SingleFrame (DoubleX4# b1 b2 b3 b4)
      = unsafeCoerce# (DoubleX4# ((-##) a1 b1) ((-##) a2 b2) ((-##) a3 b3) ((-##) a4 b4))
    {-# INLINE (-) #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) * SingleFrame (DoubleX4# b1 b2 b3 b4)
      = unsafeCoerce# (DoubleX4# ((*##) a1 b1) ((*##) a2 b2) ((*##) a3 b3) ((*##) a4 b4))
    {-# INLINE (*) #-}

    negate (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (negateDouble# a1) (negateDouble# a2) (negateDouble# a3) (negateDouble# a4))
    {-# INLINE negate #-}

    abs (SingleFrame (DoubleX4# a1 a2 a3 a4))
      = unsafeCoerce# (DoubleX4#
      (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1))
      (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
      (if isTrue# (a3 >=## 0.0##) then a3 else negateDouble# a3)
      (if isTrue# (a4 >=## 0.0##) then a4 else negateDouble# a4)
    {-# INLINE abs #-}

    signum (SingleFrame (DoubleX4# a1 a2 a3 a4))
      = unsafeCoerce# (DoubleX4# (if isTrue# (a1 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a2 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a3 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a3 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a4 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a4 <## 0.0##) then -1.0## else 0.0## ))
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of D# x -> SingleFrame (DoubleX4# x x x x)
    {-# INLINE fromInteger #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Fractional (DataFrame Double '[4]) where

    SingleFrame (DoubleX4# a1 a2 a3 a4) / SingleFrame (DoubleX4# b1 b2 b3 b4) = unsafeCoerce# (DoubleX4#
      ((/##) a1 b1) ((/##) a2 b2) ((/##) a3 b3) ((/##) a4 b4))
    {-# INLINE (/) #-}

    recip (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      ((/##) 1.0## a1) ((/##) 1.0## a2) ((/##) 1.0## a3) ((/##) 1.0## a4))
    {-# INLINE recip #-}

    fromRational r = case fromRational r of D# x -> SingleFrame (DoubleX4# x x x x)
    {-# INLINE fromRational #-}



instance {-# OVERLAPPING #-} IS_ARRAY => Floating (DataFrame Double '[4]) where

    pi = unsafeCoerce# (DoubleX4#
      3.141592653589793238##)
      3.141592653589793238##
      3.141592653589793238##
      3.141592653589793238##
    {-# INLINE pi #-}

    exp (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (expDouble# a1) (expDouble# a2) (expDouble# a3) (expDouble# a4))
    {-# INLINE exp #-}

    log (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (logDouble# a1) (logDouble# a2) (logDouble# a3) (logDouble# a4))
    {-# INLINE log #-}

    sqrt (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (sqrtDouble# a1) (sqrtDouble# a2) (sqrtDouble# a3) (sqrtDouble# a4))
    {-# INLINE sqrt #-}

    sin (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (sinDouble# a1) (sinDouble# a2) (sinDouble# a3) (sinDouble# a4))
    {-# INLINE sin #-}

    cos (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (cosDouble# a1) (cosDouble# a2) (cosDouble# a3) (cosDouble# a4))
    {-# INLINE cos #-}

    tan (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (tanDouble# a1) (tanDouble# a2) (tanDouble# a3) (tanDouble# a4))
    {-# INLINE tan #-}

    asin (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (asinDouble# a1) (asinDouble# a2) (asinDouble# a3) (asinDouble# a4))
    {-# INLINE asin #-}

    acos (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (acosDouble# a1) (acosDouble# a2) (acosDouble# a3) (acosDouble# a4))
    {-# INLINE acos #-}

    atan (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (atanDouble# a1) (atanDouble# a2) (atanDouble# a3) (atanDouble# a4))
    {-# INLINE atan #-}

    sinh (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (sinhDouble# a1) (sinhDouble# a2) (sinhDouble# a3) (sinhDouble# a4))
    {-# INLINE sinh #-}

    cosh (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (coshDouble# a1) (coshDouble# a2) (coshDouble# a3) (coshDouble# a4))
    {-# INLINE cosh #-}

    tanh (SingleFrame (DoubleX4# a1 a2 a3 a4)) = unsafeCoerce# (DoubleX4#
      (tanhDouble# a1) (tanhDouble# a2) (tanhDouble# a3) (tanhDouble# a4))
    {-# INLINE tanh #-}

    SingleFrame (DoubleX4# a1 a2 a3 a4) ** SingleFrame (DoubleX4# b1 b2 b3 b4) = unsafeCoerce# (DoubleX4#
      ((**##) a1 b1) ((**##) a2 b2) ((**##) a3 b3) ((**##) a4 b4))
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
#define ELEM_N 4

instance {-# OVERLAPPING #-} IS_ARRAY => PrimBytes (DataFrame Double '[4]) where

    getBytes (SingleFrame (DoubleX4# a1 a2 a3 a4)) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @(DataFrame Double '[4]) undefined) s0 of
           (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
             s2 -> case writeDoubleArray# marr 1# a2 s2 of
               s3 -> case writeDoubleArray# marr 2# a3 s3 of
                 s4 -> case writeDoubleArray# marr 3# a4 s4 of
                   s5 -> unsafeFreezeByteArray# marr s5
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = unsafeCoerce# (DoubleX4#
      (indexDoubleArray# arr i))
      (indexDoubleArray# arr (i +# 1#))
      (indexDoubleArray# arr (i +# 2#))
      (indexDoubleArray# arr (i +# 3#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readDoubleArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> case readDoubleArray# mba (i +# 3#) s3 of
            (# s4, a4 #) -> (# s4, SingleFrame (DoubleX4# a1 a2 a3 a4) #)
    {-# INLINE readBytes #-}

    writeBytes mba off (SingleFrame (DoubleX4# a1 a2 a3 a4)) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeDoubleArray# mba (i +# 3#) a4
      ( writeDoubleArray# mba (i +# 2#) a3
      ( writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )))
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readDoubleOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readDoubleOffAddr# addr 1# s1 of
        (# s2, a2 #) -> case readDoubleOffAddr# addr 2# s2 of
          (# s3, a3 #) -> case readDoubleOffAddr# addr 3# s3 of
            (# s4, a4 #) -> (# s4, SingleFrame (DoubleX4# a1 a2 a3 a4) #)
    {-# INLINE readAddr #-}

    writeAddr (SingleFrame (DoubleX4# a1 a2 a3 a4)) addr s
      = writeDoubleOffAddr# addr 3# a4
      ( writeDoubleOffAddr# addr 2# a3
      ( writeDoubleOffAddr# addr 1# a2
      ( writeDoubleOffAddr# addr 0# a1 s )))
    {-# INLINE writeAddr #-}

    byteSize _ = byteSize @Double undefined *# ELEM_N#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Double undefined
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    indexArray ba off
      | i <- off *# ELEM_N#
      = unsafeCoerce# (DoubleX4#
      (indexDoubleArray# ba i))
      (indexDoubleArray# ba (i +# 1#))
      (indexDoubleArray# ba (i +# 2#))
      (indexDoubleArray# ba (i +# 3#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readDoubleArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> case readDoubleArray# mba (i +# 3#) s3 of
            (# s4, a4 #) -> (# s4, SingleFrame (DoubleX4# a1 a2 a3 a4) #)
    {-# INLINE readArray #-}

    writeArray mba off (SingleFrame (DoubleX4# a1 a2 a3 a4)) s
      | i <- off *# ELEM_N#
      = writeDoubleArray# mba (i +# 3#) a4
      ( writeDoubleArray# mba (i +# 2#) a3
      ( writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )))
    {-# INLINE writeArray #-}


instance {-# OVERLAPPING #-} IS_ARRAY => PrimArray Double (DataFrame Double '[4]) where

    broadcast (D# x) = SingleFrame (DoubleX4# x x x x)
    {-# INLINE broadcast #-}

    ix# 0# (SingleFrame (DoubleX4# a1 _ _ _)) = D# a1
    ix# 1# (SingleFrame (DoubleX4# _ a2 _ _)) = D# a2
    ix# 2# (SingleFrame (DoubleX4# _ _ a3 _)) = D# a3
    ix# 3# (SingleFrame (DoubleX4# _ _ _ a4)) = D# a4
    ix# _   _                   = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, D# a1 #) -> case f s1 of
        (# s2, D# a2 #) -> case f s2 of
          (# s3, D# a3 #) -> case f s3 of
            (# s4, D# a4 #) -> (# s4, SingleFrame (DoubleX4# a1 a2 a3 a4) #)

    upd# _ 0# (D# q) (SingleFrame (DoubleX4# _ y z w)) = SingleFrame (DoubleX4# q y z w)
    upd# _ 1# (D# q) (SingleFrame (DoubleX4# x _ z w)) = SingleFrame (DoubleX4# x q z w)
    upd# _ 2# (D# q) (SingleFrame (DoubleX4# x y _ w)) = SingleFrame (DoubleX4# x y q w)
    upd# _ 3# (D# q) (SingleFrame (DoubleX4# x y z _)) = SingleFrame (DoubleX4# x y z q)
    upd# _ _ _ x                         = x
    {-# INLINE upd# #-}

    elemOffset _ = 0#
    {-# INLINE elemOffset #-}

    elemSize0 _  = ELEM_N#
    {-# INLINE elemSize0 #-}

    fromElems off _ ba = unsafeCoerce# (DoubleX4#
      (indexDoubleArray# ba off))
      (indexDoubleArray# ba (off +# 1#))
      (indexDoubleArray# ba (off +# 2#))
      (indexDoubleArray# ba (off +# 3#))
    {-# INLINE fromElems #-}
