{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedTuples         #-}
module Numeric.DataFrame.Internal.Backend.Family.DoubleX3 (DoubleX3 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.Backend.Family.PrimOps
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive                  as NonTransitive
import qualified Numeric.ProductOrd.Partial                        as Partial


data DoubleX3 = DoubleX3# Double# Double# Double#


instance Bounded DoubleX3 where
    maxBound = case inftyD of D# x -> DoubleX3# x x x
    minBound = case negate inftyD of D# x -> DoubleX3# x x x


instance Show DoubleX3 where
    show (DoubleX3# a1 a2 a3)
      =  "{ " ++ show (D# a1)
      ++ ", " ++ show (D# a2)
      ++ ", " ++ show (D# a3)
      ++ " }"



instance Eq DoubleX3 where

    DoubleX3# a1 a2 a3 == DoubleX3# b1 b2 b3 =
      isTrue#
      (       (a1 ==## b1)
      `andI#` (a2 ==## b2)
      `andI#` (a3 ==## b3)
      )
    {-# INLINE (==) #-}

    DoubleX3# a1 a2 a3 /= DoubleX3# b1 b2 b3 =
      isTrue#
      (      (a1 /=## b1)
      `orI#` (a2 /=## b2)
      `orI#` (a3 /=## b3)
      )
    {-# INLINE (/=) #-}



cmp' :: Double# -> Double# -> PartialOrdering
cmp' a b
  | isTrue# (a >## b) = PGT
  | isTrue# (a <## b) = PLT
  | otherwise  = PEQ

instance ProductOrder DoubleX3 where
    cmp (DoubleX3# a1 a2 a3) (DoubleX3# b1 b2 b3)
      = cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3
    {-# INLINE cmp #-}

instance Ord (NonTransitive.ProductOrd DoubleX3) where
    NonTransitive.ProductOrd x > NonTransitive.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    NonTransitive.ProductOrd x < NonTransitive.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (NonTransitive.ProductOrd (DoubleX3# a1 a2 a3))
         (NonTransitive.ProductOrd (DoubleX3# b1 b2 b3)) = isTrue#
      ((a1 >=## b1) `andI#` (a2 >=## b2) `andI#` (a3 >=## b3))
    {-# INLINE (>=) #-}
    (<=) (NonTransitive.ProductOrd (DoubleX3# a1 a2 a3))
         (NonTransitive.ProductOrd (DoubleX3# b1 b2 b3)) = isTrue#
      ((a1 <=## b1) `andI#` (a2 <=## b2) `andI#` (a3 <=## b3))
    {-# INLINE (<=) #-}
    compare (NonTransitive.ProductOrd a) (NonTransitive.ProductOrd b)
      = NonTransitive.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (NonTransitive.ProductOrd (DoubleX3# a1 a2 a3))
        (NonTransitive.ProductOrd (DoubleX3# b1 b2 b3))
      = NonTransitive.ProductOrd
        ( DoubleX3#
          (if isTrue# (a1 >## b1) then b1 else a1)
          (if isTrue# (a2 >## b2) then b2 else a2)
          (if isTrue# (a3 >## b3) then b3 else a3)
        )
    {-# INLINE min #-}
    max (NonTransitive.ProductOrd (DoubleX3# a1 a2 a3))
        (NonTransitive.ProductOrd (DoubleX3# b1 b2 b3))
      = NonTransitive.ProductOrd
        ( DoubleX3#
          (if isTrue# (a1 <## b1) then b1 else a1)
          (if isTrue# (a2 <## b2) then b2 else a2)
          (if isTrue# (a3 <## b3) then b3 else a3)
        )
    {-# INLINE max #-}

instance Ord (Partial.ProductOrd DoubleX3) where
    Partial.ProductOrd x > Partial.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    Partial.ProductOrd x < Partial.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (Partial.ProductOrd (DoubleX3# a1 a2 a3))
         (Partial.ProductOrd (DoubleX3# b1 b2 b3)) = isTrue#
      ((a1 >=## b1) `andI#` (a2 >=## b2) `andI#` (a3 >=## b3))
    {-# INLINE (>=) #-}
    (<=) (Partial.ProductOrd (DoubleX3# a1 a2 a3))
         (Partial.ProductOrd (DoubleX3# b1 b2 b3)) = isTrue#
      ((a1 <=## b1) `andI#` (a2 <=## b2) `andI#` (a3 <=## b3))
    {-# INLINE (<=) #-}
    compare (Partial.ProductOrd a) (Partial.ProductOrd b)
      = Partial.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (Partial.ProductOrd (DoubleX3# a1 a2 a3))
        (Partial.ProductOrd (DoubleX3# b1 b2 b3))
      = Partial.ProductOrd
        ( DoubleX3#
          (if isTrue# (a1 >## b1) then b1 else a1)
          (if isTrue# (a2 >## b2) then b2 else a2)
          (if isTrue# (a3 >## b3) then b3 else a3)
        )
    {-# INLINE min #-}
    max (Partial.ProductOrd (DoubleX3# a1 a2 a3))
        (Partial.ProductOrd (DoubleX3# b1 b2 b3))
      = Partial.ProductOrd
        ( DoubleX3#
          (if isTrue# (a1 <## b1) then b1 else a1)
          (if isTrue# (a2 <## b2) then b2 else a2)
          (if isTrue# (a3 <## b3) then b3 else a3)
        )
    {-# INLINE max #-}

instance Ord DoubleX3 where
    DoubleX3# a1 a2 a3 > DoubleX3# b1 b2 b3
      | isTrue# (a1 >## b1) = True
      | isTrue# (a1 <## b1) = False
      | isTrue# (a2 >## b2) = True
      | isTrue# (a2 <## b2) = False
      | isTrue# (a3 >## b3) = True
      | otherwise           = False
    {-# INLINE (>) #-}

    DoubleX3# a1 a2 a3 < DoubleX3# b1 b2 b3
      | isTrue# (a1 <## b1) = True
      | isTrue# (a1 >## b1) = False
      | isTrue# (a2 <## b2) = True
      | isTrue# (a2 >## b2) = False
      | isTrue# (a3 <## b3) = True
      | otherwise           = False
    {-# INLINE (<) #-}

    DoubleX3# a1 a2 a3 >= DoubleX3# b1 b2 b3
      | isTrue# (a1 <## b1) = False
      | isTrue# (a1 >## b1) = True
      | isTrue# (a2 <## b2) = False
      | isTrue# (a2 >## b2) = True
      | isTrue# (a3 <## b3) = False
      | otherwise           = True
    {-# INLINE (>=) #-}

    DoubleX3# a1 a2 a3 <= DoubleX3# b1 b2 b3
      | isTrue# (a1 >## b1) = False
      | isTrue# (a1 <## b1) = True
      | isTrue# (a2 >## b2) = False
      | isTrue# (a2 <## b2) = True
      | isTrue# (a3 >## b3) = False
      | otherwise           = True
    {-# INLINE (<=) #-}

    compare (DoubleX3# a1 a2 a3) (DoubleX3# b1 b2 b3)
      | isTrue# (a1 >## b1) = GT
      | isTrue# (a1 <## b1) = LT
      | isTrue# (a2 >## b2) = GT
      | isTrue# (a2 <## b2) = LT
      | isTrue# (a3 >## b3) = GT
      | isTrue# (a3 <## b3) = LT
      | otherwise           = EQ
    {-# INLINE compare #-}




-- | element-wise operations for vectors
instance Num DoubleX3 where

    DoubleX3# a1 a2 a3 + DoubleX3# b1 b2 b3
      = DoubleX3# ((+##) a1 b1) ((+##) a2 b2) ((+##) a3 b3)
    {-# INLINE (+) #-}

    DoubleX3# a1 a2 a3 - DoubleX3# b1 b2 b3
      = DoubleX3# ((-##) a1 b1) ((-##) a2 b2) ((-##) a3 b3)
    {-# INLINE (-) #-}

    DoubleX3# a1 a2 a3 * DoubleX3# b1 b2 b3
      = DoubleX3# ((*##) a1 b1) ((*##) a2 b2) ((*##) a3 b3)
    {-# INLINE (*) #-}

    negate (DoubleX3# a1 a2 a3) = DoubleX3#
      (negateDouble# a1) (negateDouble# a2) (negateDouble# a3)
    {-# INLINE negate #-}

    abs (DoubleX3# a1 a2 a3)
      = DoubleX3#
      (if isTrue# (a1 >=## 0.0##) then a1 else negateDouble# a1)
      (if isTrue# (a2 >=## 0.0##) then a2 else negateDouble# a2)
      (if isTrue# (a3 >=## 0.0##) then a3 else negateDouble# a3)
    {-# INLINE abs #-}

    signum (DoubleX3# a1 a2 a3)
      = DoubleX3# (if isTrue# (a1 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a1 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a2 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a2 <## 0.0##) then -1.0## else 0.0## )
                 (if isTrue# (a3 >## 0.0##)
                  then 1.0##
                  else if isTrue# (a3 <## 0.0##) then -1.0## else 0.0## )
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of D# x -> DoubleX3# x x x
    {-# INLINE fromInteger #-}



instance Fractional DoubleX3 where

    DoubleX3# a1 a2 a3 / DoubleX3# b1 b2 b3 = DoubleX3#
      ((/##) a1 b1) ((/##) a2 b2) ((/##) a3 b3)
    {-# INLINE (/) #-}

    recip (DoubleX3# a1 a2 a3) = DoubleX3#
      ((/##) 1.0## a1) ((/##) 1.0## a2) ((/##) 1.0## a3)
    {-# INLINE recip #-}

    fromRational r = case fromRational r of D# x -> DoubleX3# x x x
    {-# INLINE fromRational #-}



instance Floating DoubleX3 where

    pi = DoubleX3#
      3.141592653589793238##
      3.141592653589793238##
      3.141592653589793238##
    {-# INLINE pi #-}

    exp (DoubleX3# a1 a2 a3) = DoubleX3#
      (expDouble# a1) (expDouble# a2) (expDouble# a3)
    {-# INLINE exp #-}

    log (DoubleX3# a1 a2 a3) = DoubleX3#
      (logDouble# a1) (logDouble# a2) (logDouble# a3)
    {-# INLINE log #-}

    sqrt (DoubleX3# a1 a2 a3) = DoubleX3#
      (sqrtDouble# a1) (sqrtDouble# a2) (sqrtDouble# a3)
    {-# INLINE sqrt #-}

    sin (DoubleX3# a1 a2 a3) = DoubleX3#
      (sinDouble# a1) (sinDouble# a2) (sinDouble# a3)
    {-# INLINE sin #-}

    cos (DoubleX3# a1 a2 a3) = DoubleX3#
      (cosDouble# a1) (cosDouble# a2) (cosDouble# a3)
    {-# INLINE cos #-}

    tan (DoubleX3# a1 a2 a3) = DoubleX3#
      (tanDouble# a1) (tanDouble# a2) (tanDouble# a3)
    {-# INLINE tan #-}

    asin (DoubleX3# a1 a2 a3) = DoubleX3#
      (asinDouble# a1) (asinDouble# a2) (asinDouble# a3)
    {-# INLINE asin #-}

    acos (DoubleX3# a1 a2 a3) = DoubleX3#
      (acosDouble# a1) (acosDouble# a2) (acosDouble# a3)
    {-# INLINE acos #-}

    atan (DoubleX3# a1 a2 a3) = DoubleX3#
      (atanDouble# a1) (atanDouble# a2) (atanDouble# a3)
    {-# INLINE atan #-}

    sinh (DoubleX3# a1 a2 a3) = DoubleX3#
      (sinhDouble# a1) (sinhDouble# a2) (sinhDouble# a3)
    {-# INLINE sinh #-}

    cosh (DoubleX3# a1 a2 a3) = DoubleX3#
      (coshDouble# a1) (coshDouble# a2) (coshDouble# a3)
    {-# INLINE cosh #-}

    tanh (DoubleX3# a1 a2 a3) = DoubleX3#
      (tanhDouble# a1) (tanhDouble# a2) (tanhDouble# a3)
    {-# INLINE tanh #-}

    DoubleX3# a1 a2 a3 ** DoubleX3# b1 b2 b3 = DoubleX3#
      ((**##) a1 b1) ((**##) a2 b2) ((**##) a3 b3)
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
#define ELEM_N 3

instance PrimBytes DoubleX3 where

    getBytes (DoubleX3# a1 a2 a3) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @DoubleX3 undefined) s0 of
           (# s1, marr #) -> case writeDoubleArray# marr 0# a1 s1 of
             s2 -> case writeDoubleArray# marr 1# a2 s2 of
               s3 -> case writeDoubleArray# marr 2# a3 s3 of
                 s4 -> unsafeFreezeByteArray# marr s4
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = DoubleX3#
      (indexDoubleArray# arr i)
      (indexDoubleArray# arr (i +# 1#))
      (indexDoubleArray# arr (i +# 2#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readDoubleArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> (# s3, DoubleX3# a1 a2 a3 #)
    {-# INLINE readBytes #-}

    writeBytes mba off (DoubleX3# a1 a2 a3) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeDoubleArray# mba (i +# 2#) a3
      ( writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s ))
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readDoubleOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readDoubleOffAddr# addr 1# s1 of
        (# s2, a2 #) -> case readDoubleOffAddr# addr 2# s2 of
          (# s3, a3 #) -> (# s3, DoubleX3# a1 a2 a3 #)
    {-# INLINE readAddr #-}

    writeAddr (DoubleX3# a1 a2 a3) addr s
      = writeDoubleOffAddr# addr 2# a3
      ( writeDoubleOffAddr# addr 1# a2
      ( writeDoubleOffAddr# addr 0# a1 s ))
    {-# INLINE writeAddr #-}

    byteSize _ = byteSize @Double undefined *# ELEM_N#
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign @Double undefined
    {-# INLINE byteAlign #-}

    byteOffset _ = 0#
    {-# INLINE byteOffset #-}

    indexArray ba off
      | i <- off *# ELEM_N#
      = DoubleX3#
      (indexDoubleArray# ba i)
      (indexDoubleArray# ba (i +# 1#))
      (indexDoubleArray# ba (i +# 2#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readDoubleArray# mba i s0 of
      (# s1, a1 #) -> case readDoubleArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> case readDoubleArray# mba (i +# 2#) s2 of
          (# s3, a3 #) -> (# s3, DoubleX3# a1 a2 a3 #)
    {-# INLINE readArray #-}

    writeArray mba off (DoubleX3# a1 a2 a3) s
      | i <- off *# ELEM_N#
      = writeDoubleArray# mba (i +# 2#) a3
      ( writeDoubleArray# mba (i +# 1#) a2
      ( writeDoubleArray# mba  i        a1 s ))
    {-# INLINE writeArray #-}


instance PrimArray Double DoubleX3 where

    broadcast (D# x) = DoubleX3# x x x
    {-# INLINE broadcast #-}

    ix# 0# (DoubleX3# a1 _ _) = D# a1
    ix# 1# (DoubleX3# _ a2 _) = D# a2
    ix# 2# (DoubleX3# _ _ a3) = D# a3
    ix# _   _                 = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, D# a1 #) -> case f s1 of
        (# s2, D# a2 #) -> case f s2 of
          (# s3, D# a3 #) -> (# s3, DoubleX3# a1 a2 a3 #)


    upd# _ 0# (D# q) (DoubleX3# _ y z) = DoubleX3# q y z
    upd# _ 1# (D# q) (DoubleX3# x _ z) = DoubleX3# x q z
    upd# _ 2# (D# q) (DoubleX3# x y _) = DoubleX3# x y q
    upd# _ _ _ x                       = x
    {-# INLINE upd# #-}

    offsetElems _ = 0#
    {-# INLINE offsetElems #-}

    uniqueOrCumulDims _ = Right (CumulDims [ELEM_N, 1])
    {-# INLINE uniqueOrCumulDims #-}

    fromElems _ off ba = DoubleX3#
      (indexDoubleArray# ba off)
      (indexDoubleArray# ba (off +# 1#))
      (indexDoubleArray# ba (off +# 2#))
    {-# INLINE fromElems #-}
