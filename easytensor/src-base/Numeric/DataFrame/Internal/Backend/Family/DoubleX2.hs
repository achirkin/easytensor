{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedSums           #-}
{-# LANGUAGE UnboxedTuples         #-}
module Numeric.DataFrame.Internal.Backend.Family.DoubleX2 (DoubleX2 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Backend.Family.PrimOps
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive                  as NonTransitive
import qualified Numeric.ProductOrd.Partial                        as Partial


data DoubleX2 = DoubleX2# Double# Double#


instance Bounded DoubleX2 where
    maxBound = case inftyD of D# x -> DoubleX2# x x
    minBound = case negate inftyD of D# x -> DoubleX2# x x


instance Eq DoubleX2 where

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



cmp' :: Double# -> Double# -> PartialOrdering
cmp' a b
  | isTrue# (a >## b) = PGT
  | isTrue# (a <## b) = PLT
  | otherwise  = PEQ

instance ProductOrder DoubleX2 where
    cmp (DoubleX2# a1 a2) (DoubleX2# b1 b2)
      = cmp' a1 b1 <> cmp' a2 b2
    {-# INLINE cmp #-}

instance Ord (NonTransitive.ProductOrd DoubleX2) where
    NonTransitive.ProductOrd x > NonTransitive.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    NonTransitive.ProductOrd x < NonTransitive.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (NonTransitive.ProductOrd (DoubleX2# a1 a2))
         (NonTransitive.ProductOrd (DoubleX2# b1 b2)) = isTrue#
      ((a1 >=## b1) `andI#` (a2 >=## b2))
    {-# INLINE (>=) #-}
    (<=) (NonTransitive.ProductOrd (DoubleX2# a1 a2))
         (NonTransitive.ProductOrd (DoubleX2# b1 b2)) = isTrue#
      ((a1 <=## b1) `andI#` (a2 <=## b2))
    {-# INLINE (<=) #-}
    compare (NonTransitive.ProductOrd a) (NonTransitive.ProductOrd b)
      = NonTransitive.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (NonTransitive.ProductOrd (DoubleX2# a1 a2))
        (NonTransitive.ProductOrd (DoubleX2# b1 b2))
      = NonTransitive.ProductOrd
        ( DoubleX2#
          (if isTrue# (a1 >## b1) then b1 else a1)
          (if isTrue# (a2 >## b2) then b2 else a2)
        )
    {-# INLINE min #-}
    max (NonTransitive.ProductOrd (DoubleX2# a1 a2))
        (NonTransitive.ProductOrd (DoubleX2# b1 b2))
      = NonTransitive.ProductOrd
        ( DoubleX2#
          (if isTrue# (a1 <## b1) then b1 else a1)
          (if isTrue# (a2 <## b2) then b2 else a2)
        )
    {-# INLINE max #-}

instance Ord (Partial.ProductOrd DoubleX2) where
    Partial.ProductOrd x > Partial.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    Partial.ProductOrd x < Partial.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (Partial.ProductOrd (DoubleX2# a1 a2))
         (Partial.ProductOrd (DoubleX2# b1 b2)) = isTrue#
      ((a1 >=## b1) `andI#` (a2 >=## b2))
    {-# INLINE (>=) #-}
    (<=) (Partial.ProductOrd (DoubleX2# a1 a2))
         (Partial.ProductOrd (DoubleX2# b1 b2)) = isTrue#
      ((a1 <=## b1) `andI#` (a2 <=## b2))
    {-# INLINE (<=) #-}
    compare (Partial.ProductOrd a) (Partial.ProductOrd b)
      = Partial.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (Partial.ProductOrd (DoubleX2# a1 a2))
        (Partial.ProductOrd (DoubleX2# b1 b2))
      = Partial.ProductOrd
        ( DoubleX2#
          (if isTrue# (a1 >## b1) then b1 else a1)
          (if isTrue# (a2 >## b2) then b2 else a2)
        )
    {-# INLINE min #-}
    max (Partial.ProductOrd (DoubleX2# a1 a2))
        (Partial.ProductOrd (DoubleX2# b1 b2))
      = Partial.ProductOrd
        ( DoubleX2#
          (if isTrue# (a1 <## b1) then b1 else a1)
          (if isTrue# (a2 <## b2) then b2 else a2)
        )
    {-# INLINE max #-}

instance Ord DoubleX2 where
    DoubleX2# a1 a2 > DoubleX2# b1 b2
      | isTrue# (a1 >## b1) = True
      | isTrue# (a1 <## b1) = False
      | isTrue# (a2 >## b2) = True
      | otherwise           = False
    {-# INLINE (>) #-}

    DoubleX2# a1 a2 < DoubleX2# b1 b2
      | isTrue# (a1 <## b1) = True
      | isTrue# (a1 >## b1) = False
      | isTrue# (a2 <## b2) = True
      | otherwise           = False
    {-# INLINE (<) #-}

    DoubleX2# a1 a2 >= DoubleX2# b1 b2
      | isTrue# (a1 <## b1) = False
      | isTrue# (a1 >## b1) = True
      | isTrue# (a2 <## b2) = False
      | otherwise           = True
    {-# INLINE (>=) #-}

    DoubleX2# a1 a2 <= DoubleX2# b1 b2
      | isTrue# (a1 >## b1) = False
      | isTrue# (a1 <## b1) = True
      | isTrue# (a2 >## b2) = False
      | otherwise           = True
    {-# INLINE (<=) #-}

    compare (DoubleX2# a1 a2) (DoubleX2# b1 b2)
      | isTrue# (a1 >## b1) = GT
      | isTrue# (a1 <## b1) = LT
      | isTrue# (a2 >## b2) = GT
      | isTrue# (a2 <## b2) = LT
      | otherwise           = EQ
    {-# INLINE compare #-}



-- | element-wise operations for vectors
instance Num DoubleX2 where

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



instance Fractional DoubleX2 where

    DoubleX2# a1 a2 / DoubleX2# b1 b2 = DoubleX2#
      ((/##) a1 b1) ((/##) a2 b2)
    {-# INLINE (/) #-}

    recip (DoubleX2# a1 a2) = DoubleX2#
      ((/##) 1.0## a1) ((/##) 1.0## a2)
    {-# INLINE recip #-}

    fromRational r = case fromRational r of D# x -> DoubleX2# x x
    {-# INLINE fromRational #-}



instance Floating DoubleX2 where

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

-- offset in bytes is S times bigger than offset in prim elements,
-- when S is power of two, this is equal to shift
#define BOFF_TO_PRIMOFF(off) uncheckedIShiftRL# off 3#
#define ELEM_N 2

instance PrimBytes DoubleX2 where

    getBytes (DoubleX2# a1 a2) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @DoubleX2 undefined) s0 of
           (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
             s2 -> case writeDoubleArray# marr 1# a2 s2 of
               s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = DoubleX2#
      (indexDoubleArray# arr i)
      (indexDoubleArray# arr (i +# 1#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, DoubleX2# a1 a2 #)
    {-# INLINE readBytes #-}

    writeBytes mba off (DoubleX2# a1 a2) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readDoubleOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readDoubleOffAddr# addr 1# s1 of
        (# s2, a2 #) -> (# s2, DoubleX2# a1 a2 #)
    {-# INLINE readAddr #-}

    writeAddr (DoubleX2# a1 a2) addr s
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
      = DoubleX2#
      (indexDoubleArray# ba i)
      (indexDoubleArray# ba (i +# 1#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, DoubleX2# a1 a2 #)
    {-# INLINE readArray #-}

    writeArray mba off (DoubleX2# a1 a2) s
      | i <- off *# ELEM_N#
      = writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s )
    {-# INLINE writeArray #-}


instance PrimArray Double DoubleX2 where

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
    upd# _ _ _ x                     = x
    {-# INLINE upd# #-}

    arrayContent# x = (# | (# CumulDims [ELEM_N, 1], 0#, getBytes x #) #)
    {-# INLINE arrayContent# #-}

    offsetElems _ = 0#
    {-# INLINE offsetElems #-}

    uniqueOrCumulDims _ = Right (CumulDims [ELEM_N, 1])
    {-# INLINE uniqueOrCumulDims #-}

    fromElems _ off ba = DoubleX2#
      (indexDoubleArray# ba off)
      (indexDoubleArray# ba (off +# 1#))
    {-# INLINE fromElems #-}
