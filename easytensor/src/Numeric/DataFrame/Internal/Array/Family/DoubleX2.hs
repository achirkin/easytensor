{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedTuples         #-}
module Numeric.DataFrame.Internal.Array.Family.DoubleX2 (DoubleX2 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.PrimBytes


data DoubleX2 k = DoubleX2# Double# Double#


instance Bounded (DoubleX2 k) where
    maxBound = case infty of D# x -> DoubleX2# x x
    minBound = case negate infty of D# x -> DoubleX2# x x

infty :: Double
infty = read "Infinity"

instance Show (DoubleX2 k) where
    show (DoubleX2# a1 a2)
      =  "{ " ++ show (D# a1)
      ++ ", " ++ show (D# a2)
      ++ " }"



instance Eq (DoubleX2 k) where

    DoubleX2# a1 a2 == DoubleX2# b1 b2 =
      isTrue#
      (       (a1 ==## b1)
      `andI#` (a2 ==## b2)
      )
    {-# INLINE (==) #-}

    DoubleX2# a1 a2 /= DoubleX2# b1 b2 =
      isTrue#
      (      (a1 /=## b1)
      `orI#` (a2 /=## b2)
      )
    {-# INLINE (/=) #-}



-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--           and lexicographical ordering for `compare`
instance Ord (DoubleX2 k) where
    DoubleX2# a1 a2 > DoubleX2# b1 b2 =
      isTrue#
      (       (a1 >## b1)
      `andI#` (a2 >## b2)
      )
    {-# INLINE (>) #-}

    DoubleX2# a1 a2 < DoubleX2# b1 b2 =
      isTrue#
      (       (a1 <## b1)
      `andI#` (a2 <## b2)
      )
    {-# INLINE (<) #-}

    DoubleX2# a1 a2 >= DoubleX2# b1 b2 =
      isTrue#
      (       (a1 >=## b1)
      `andI#` (a2 >=## b2)
      )
    {-# INLINE (>=) #-}

    DoubleX2# a1 a2 <= DoubleX2# b1 b2 =
      isTrue#
      (       (a1 <=## b1)
      `andI#` (a2 <=## b2)
      )
    {-# INLINE (<=) #-}

    -- | Compare lexicographically
    compare (DoubleX2# a1 a2) (DoubleX2# b1 b2)
      | isTrue# (a1 >## b1) = GT
      | isTrue# (a1 <## b1) = LT
      | isTrue# (a2 >## b2) = GT
      | isTrue# (a2 <## b2) = LT
      | otherwise = EQ
    {-# INLINE compare #-}

    -- | Element-wise minimum
    min (DoubleX2# a1 a2) (DoubleX2# b1 b2) = DoubleX2#
      (if isTrue# (a1 >## b1) then b1 else a1)
      (if isTrue# (a2 >## b2) then b2 else a2)
    {-# INLINE min #-}

    -- | Element-wise maximum
    max (DoubleX2# a1 a2) (DoubleX2# b1 b2) = DoubleX2#
      (if isTrue# (a1 >## b1) then a1 else b1)
      (if isTrue# (a2 >## b2) then a2 else b2)
    {-# INLINE max #-}



-- | element-wise operations for vectors
instance Num (DoubleX2 k) where

    DoubleX2# a1 a2 + DoubleX2# b1 b2
      = DoubleX2# ((+##) a1 b1) ((+##) a2 b2)
    {-# INLINE (+) #-}

    DoubleX2# a1 a2 - DoubleX2# b1 b2
      = DoubleX2# ((-##) a1 b1) ((-##) a2 b2)
    {-# INLINE (-) #-}

    DoubleX2# a1 a2 * DoubleX2# b1 b2
      = DoubleX2# ((*##) a1 b1) ((*##) a2 b2)
    {-# INLINE (*) #-}

    negate (DoubleX2# a1 a2) = DoubleX2#
      (negateDouble# a1) (negateDouble# a2)
    {-# INLINE negate #-}

    abs (DoubleX2# a1 a2)
      = DoubleX2#
      (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1)
      (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
    {-# INLINE abs #-}

    signum (DoubleX2# a1 a2)
      = DoubleX2# (if isTrue# (a1 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a2 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of D# x -> DoubleX2# x x
    {-# INLINE fromInteger #-}



instance Fractional (DoubleX2 k) where

    DoubleX2# a1 a2 / DoubleX2# b1 b2 = DoubleX2#
      ((/##) a1 b1) ((/##) a2 b2)
    {-# INLINE (/) #-}

    recip (DoubleX2# a1 a2) = DoubleX2#
      ((/##) 1.0## a1) ((/##) 1.0## a2)
    {-# INLINE recip #-}

    fromRational r = case fromRational r of D# x -> DoubleX2# x x
    {-# INLINE fromRational #-}



instance Floating (DoubleX2 k) where

    pi = DoubleX2#
      3.141592653589793238##
      3.141592653589793238##
    {-# INLINE pi #-}

    exp (DoubleX2# a1 a2) = DoubleX2#
      (expDouble# a1) (expDouble# a2)
    {-# INLINE exp #-}

    log (DoubleX2# a1 a2) = DoubleX2#
      (logDouble# a1) (logDouble# a2)
    {-# INLINE log #-}

    sqrt (DoubleX2# a1 a2) = DoubleX2#
      (sqrtDouble# a1) (sqrtDouble# a2)
    {-# INLINE sqrt #-}

    sin (DoubleX2# a1 a2) = DoubleX2#
      (sinDouble# a1) (sinDouble# a2)
    {-# INLINE sin #-}

    cos (DoubleX2# a1 a2) = DoubleX2#
      (cosDouble# a1) (cosDouble# a2)
    {-# INLINE cos #-}

    tan (DoubleX2# a1 a2) = DoubleX2#
      (tanDouble# a1) (tanDouble# a2)
    {-# INLINE tan #-}

    asin (DoubleX2# a1 a2) = DoubleX2#
      (asinDouble# a1) (asinDouble# a2)
    {-# INLINE asin #-}

    acos (DoubleX2# a1 a2) = DoubleX2#
      (acosDouble# a1) (acosDouble# a2)
    {-# INLINE acos #-}

    atan (DoubleX2# a1 a2) = DoubleX2#
      (atanDouble# a1) (atanDouble# a2)
    {-# INLINE atan #-}

    sinh (DoubleX2# a1 a2) = DoubleX2#
      (sinhDouble# a1) (sinhDouble# a2)
    {-# INLINE sinh #-}

    cosh (DoubleX2# a1 a2) = DoubleX2#
      (coshDouble# a1) (coshDouble# a2)
    {-# INLINE cosh #-}

    tanh (DoubleX2# a1 a2) = DoubleX2#
      (tanhDouble# a1) (tanhDouble# a2)
    {-# INLINE tanh #-}

    DoubleX2# a1 a2 ** DoubleX2# b1 b2 = DoubleX2#
      ((**##) a1 b1) ((**##) a2 b2)
    {-# INLINE (**) #-}

    logBase x y         =  log y / log x
    {-# INLINE logBase #-}

    asinh x = log (x + sqrt (1.0+x*x))
    {-# INLINE asinh #-}

    acosh x = log (x + (x+1.0) * sqrt ((x-1.0)/(x+1.0)))
    {-# INLINE acosh #-}

    atanh x = 0.5 * log ((1.0+x) / (1.0-x))
    {-# INLINE atanh #-}




instance PrimBytes (DoubleX2 k) where

    getBytes (DoubleX2# a1 a2) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @Double undefined *# 3#) s0 of
           (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
             s2 -> case writeDoubleArray# marr 1# a2 s2 of
               s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- uncheckedIShiftRL# off 3#
      = DoubleX2#
      (indexDoubleArray# arr i)
      (indexDoubleArray# arr (i +# 1#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- uncheckedIShiftRL# off 3#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, DoubleX2# a1 a2 #)
    {-# INLINE readBytes #-}

    writeBytes mba off (DoubleX2# a1 a2) s
      | i <- uncheckedIShiftRL# off 3#
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeBytes #-}


    byteSize _ = byteSize @Double undefined *# 2#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Double undefined *# 2#
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    indexArray ba off
      | i <- off *# 2#
      = DoubleX2#
      (indexDoubleArray# ba i)
      (indexDoubleArray# ba (i +# 1#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# 2#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, DoubleX2# a1 a2 #)
    {-# INLINE readArray #-}

    writeArray mba off (DoubleX2# a1 a2) s
      | i <- off *# 2#
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeArray #-}


instance PrimArray Double (DoubleX2 k) where

    broadcast (D# x) = DoubleX2# x x
    {-# INLINE broadcast #-}

    ix# 0# (DoubleX2# a1 _) = D# a1
    ix# 1# (DoubleX2# _ a2) = D# a2
    ix# _   _               = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, D# a1 #) -> case f s1 of
        (# s2, D# a2 #) -> (# s2, DoubleX2# a1 a2 #)


    upd# _ 0# (D# q) (DoubleX2# _ y) = DoubleX2# q y
    upd# _ 1# (D# q) (DoubleX2# x _) = DoubleX2# x q
    upd# _ _ _ x                       = x
    {-# INLINE upd# #-}

    elemOffset _ = 0#
    {-# INLINE elemOffset #-}

    elemSize0 _  = 2#
    {-# INLINE elemSize0 #-}

    fromElems off _ ba = DoubleX2#
      (indexDoubleArray# ba off)
      (indexDoubleArray# ba (off +# 1#))
    {-# INLINE fromElems #-}
