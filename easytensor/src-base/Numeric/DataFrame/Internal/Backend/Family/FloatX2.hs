{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Numeric.DataFrame.Internal.Backend.Family.FloatX2 (FloatX2 (..)) where


import           GHC.Base
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive     as NonTransitive
import qualified Numeric.ProductOrd.Partial           as Partial


data FloatX2 = FloatX2# Float# Float#

-- | Since @Bounded@ is not implemented for floating point types, this instance
--   has an unresolvable constraint.
--   Nevetheless, it is good to have it here for nicer error messages.
instance Bounded Float => Bounded FloatX2 where
    maxBound = case maxBound of F# x -> FloatX2# x x
    minBound = case minBound of F# x -> FloatX2# x x


instance Eq FloatX2 where

    FloatX2# a1 a2 == FloatX2# b1 b2 =
      isTrue#
      (       (a1 `eqFloat#` b1)
      `andI#` (a2 `eqFloat#` b2)
      )
    {-# INLINE (==) #-}

    FloatX2# a1 a2 /= FloatX2# b1 b2 =
      isTrue#
      (      (a1 `neFloat#` b1)
      `orI#` (a2 `neFloat#` b2)
      )
    {-# INLINE (/=) #-}



cmp' :: Float# -> Float# -> PartialOrdering
cmp' a b
  | isTrue# (a `gtFloat#` b) = PGT
  | isTrue# (a `ltFloat#` b) = PLT
  | otherwise  = PEQ

instance ProductOrder FloatX2 where
    cmp (FloatX2# a1 a2) (FloatX2# b1 b2)
      = cmp' a1 b1 <> cmp' a2 b2
    {-# INLINE cmp #-}

instance Ord (NonTransitive.ProductOrd FloatX2) where
    NonTransitive.ProductOrd x > NonTransitive.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    NonTransitive.ProductOrd x < NonTransitive.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (NonTransitive.ProductOrd (FloatX2# a1 a2))
         (NonTransitive.ProductOrd (FloatX2# b1 b2)) = isTrue#
      ((a1 `geFloat#` b1) `andI#` (a2 `geFloat#` b2))
    {-# INLINE (>=) #-}
    (<=) (NonTransitive.ProductOrd (FloatX2# a1 a2))
         (NonTransitive.ProductOrd (FloatX2# b1 b2)) = isTrue#
      ((a1 `leFloat#` b1) `andI#` (a2 `leFloat#` b2))
    {-# INLINE (<=) #-}
    compare (NonTransitive.ProductOrd a) (NonTransitive.ProductOrd b)
      = NonTransitive.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (NonTransitive.ProductOrd (FloatX2# a1 a2))
        (NonTransitive.ProductOrd (FloatX2# b1 b2))
      = NonTransitive.ProductOrd
        ( FloatX2#
          (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
        )
    {-# INLINE min #-}
    max (NonTransitive.ProductOrd (FloatX2# a1 a2))
        (NonTransitive.ProductOrd (FloatX2# b1 b2))
      = NonTransitive.ProductOrd
        ( FloatX2#
          (if isTrue# (a1 `ltFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `ltFloat#` b2) then b2 else a2)
        )
    {-# INLINE max #-}

instance Ord (Partial.ProductOrd FloatX2) where
    Partial.ProductOrd x > Partial.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    Partial.ProductOrd x < Partial.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) (Partial.ProductOrd (FloatX2# a1 a2))
         (Partial.ProductOrd (FloatX2# b1 b2)) = isTrue#
      ((a1 `geFloat#` b1) `andI#` (a2 `geFloat#` b2))
    {-# INLINE (>=) #-}
    (<=) (Partial.ProductOrd (FloatX2# a1 a2))
         (Partial.ProductOrd (FloatX2# b1 b2)) = isTrue#
      ((a1 `leFloat#` b1) `andI#` (a2 `leFloat#` b2))
    {-# INLINE (<=) #-}
    compare (Partial.ProductOrd a) (Partial.ProductOrd b)
      = Partial.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min (Partial.ProductOrd (FloatX2# a1 a2))
        (Partial.ProductOrd (FloatX2# b1 b2))
      = Partial.ProductOrd
        ( FloatX2#
          (if isTrue# (a1 `gtFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `gtFloat#` b2) then b2 else a2)
        )
    {-# INLINE min #-}
    max (Partial.ProductOrd (FloatX2# a1 a2))
        (Partial.ProductOrd (FloatX2# b1 b2))
      = Partial.ProductOrd
        ( FloatX2#
          (if isTrue# (a1 `ltFloat#` b1) then b1 else a1)
          (if isTrue# (a2 `ltFloat#` b2) then b2 else a2)
        )
    {-# INLINE max #-}

instance Ord FloatX2 where
    FloatX2# a1 a2 > FloatX2# b1 b2
      | isTrue# (a1 `gtFloat#` b1) = True
      | isTrue# (a1 `ltFloat#` b1) = False
      | isTrue# (a2 `gtFloat#` b2) = True
      | otherwise           = False
    {-# INLINE (>) #-}

    FloatX2# a1 a2 < FloatX2# b1 b2
      | isTrue# (a1 `ltFloat#` b1) = True
      | isTrue# (a1 `gtFloat#` b1) = False
      | isTrue# (a2 `ltFloat#` b2) = True
      | otherwise           = False
    {-# INLINE (<) #-}

    FloatX2# a1 a2 >= FloatX2# b1 b2
      | isTrue# (a1 `ltFloat#` b1) = False
      | isTrue# (a1 `gtFloat#` b1) = True
      | isTrue# (a2 `ltFloat#` b2) = False
      | otherwise           = True
    {-# INLINE (>=) #-}

    FloatX2# a1 a2 <= FloatX2# b1 b2
      | isTrue# (a1 `gtFloat#` b1) = False
      | isTrue# (a1 `ltFloat#` b1) = True
      | isTrue# (a2 `gtFloat#` b2) = False
      | otherwise           = True
    {-# INLINE (<=) #-}

    compare (FloatX2# a1 a2) (FloatX2# b1 b2)
      | isTrue# (a1 `gtFloat#` b1) = GT
      | isTrue# (a1 `ltFloat#` b1) = LT
      | isTrue# (a2 `gtFloat#` b2) = GT
      | isTrue# (a2 `ltFloat#` b2) = LT
      | otherwise           = EQ
    {-# INLINE compare #-}



-- | element-wise operations for vectors
instance Num FloatX2 where

    FloatX2# a1 a2 + FloatX2# b1 b2
      = FloatX2# (plusFloat# a1 b1) (plusFloat# a2 b2)
    {-# INLINE (+) #-}

    FloatX2# a1 a2 - FloatX2# b1 b2
      = FloatX2# (minusFloat# a1 b1) (minusFloat# a2 b2)
    {-# INLINE (-) #-}

    FloatX2# a1 a2 * FloatX2# b1 b2
      = FloatX2# (timesFloat# a1 b1) (timesFloat# a2 b2)
    {-# INLINE (*) #-}

    negate (FloatX2# a1 a2) = FloatX2#
      (negateFloat# a1) (negateFloat# a2)
    {-# INLINE negate #-}

    abs (FloatX2# a1 a2)
      = FloatX2#
      (if isTrue# (a1 `geFloat#` 0.0#) then a1 else negateFloat# a1)
      (if isTrue# (a2 `geFloat#` 0.0#) then a2 else negateFloat# a2)
    {-# INLINE abs #-}

    signum (FloatX2# a1 a2)
      = FloatX2# (if isTrue# (a1 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a1 `ltFloat#` 0.0#) then -1.0# else 0.0# )
                 (if isTrue# (a2 `gtFloat#` 0.0#)
                  then 1.0#
                  else if isTrue# (a2 `ltFloat#` 0.0#) then -1.0# else 0.0# )
    {-# INLINE signum #-}

    fromInteger n = case fromInteger n of F# x -> FloatX2# x x
    {-# INLINE fromInteger #-}



instance Fractional FloatX2 where

    FloatX2# a1 a2 / FloatX2# b1 b2 = FloatX2#
      (divideFloat# a1 b1) (divideFloat# a2 b2)
    {-# INLINE (/) #-}

    recip (FloatX2# a1 a2) = FloatX2#
      (divideFloat# 1.0# a1) (divideFloat# 1.0# a2)
    {-# INLINE recip #-}

    fromRational r = case fromRational r of F# x -> FloatX2# x x
    {-# INLINE fromRational #-}



instance Floating FloatX2 where

    pi = FloatX2#
      3.141592653589793238#
      3.141592653589793238#
    {-# INLINE pi #-}

    exp (FloatX2# a1 a2) = FloatX2#
      (expFloat# a1) (expFloat# a2)
    {-# INLINE exp #-}

    log (FloatX2# a1 a2) = FloatX2#
      (logFloat# a1) (logFloat# a2)
    {-# INLINE log #-}

    sqrt (FloatX2# a1 a2) = FloatX2#
      (sqrtFloat# a1) (sqrtFloat# a2)
    {-# INLINE sqrt #-}

    sin (FloatX2# a1 a2) = FloatX2#
      (sinFloat# a1) (sinFloat# a2)
    {-# INLINE sin #-}

    cos (FloatX2# a1 a2) = FloatX2#
      (cosFloat# a1) (cosFloat# a2)
    {-# INLINE cos #-}

    tan (FloatX2# a1 a2) = FloatX2#
      (tanFloat# a1) (tanFloat# a2)
    {-# INLINE tan #-}

    asin (FloatX2# a1 a2) = FloatX2#
      (asinFloat# a1) (asinFloat# a2)
    {-# INLINE asin #-}

    acos (FloatX2# a1 a2) = FloatX2#
      (acosFloat# a1) (acosFloat# a2)
    {-# INLINE acos #-}

    atan (FloatX2# a1 a2) = FloatX2#
      (atanFloat# a1) (atanFloat# a2)
    {-# INLINE atan #-}

    sinh (FloatX2# a1 a2) = FloatX2#
      (sinhFloat# a1) (sinhFloat# a2)
    {-# INLINE sinh #-}

    cosh (FloatX2# a1 a2) = FloatX2#
      (coshFloat# a1) (coshFloat# a2)
    {-# INLINE cosh #-}

    tanh (FloatX2# a1 a2) = FloatX2#
      (tanhFloat# a1) (tanhFloat# a2)
    {-# INLINE tanh #-}

    FloatX2# a1 a2 ** FloatX2# b1 b2 = FloatX2#
      (powerFloat# a1 b1) (powerFloat# a2 b2)
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

instance PrimBytes FloatX2 where

    getBytes (FloatX2# a1 a2) = case runRW#
       ( \s0 -> case newByteArray# (byteSize @FloatX2 undefined) s0 of
           (# s1, marr #) -> case writeFloatArray# marr 0# a1 s1 of
             s2 -> case writeFloatArray# marr 1# a2 s2 of
               s3 -> unsafeFreezeByteArray# marr s3
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}

    fromBytes off arr
      | i <- BOFF_TO_PRIMOFF(off)
      = FloatX2#
      (indexFloatArray# arr i)
      (indexFloatArray# arr (i +# 1#))
    {-# INLINE fromBytes #-}

    readBytes mba off s0
      | i <- BOFF_TO_PRIMOFF(off)
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, FloatX2# a1 a2 #)
    {-# INLINE readBytes #-}

    writeBytes mba off (FloatX2# a1 a2) s
      | i <- BOFF_TO_PRIMOFF(off)
      = writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s )
    {-# INLINE writeBytes #-}

    readAddr addr s0
      = case readFloatOffAddr# addr 0# s0 of
      (# s1, a1 #) -> case readFloatOffAddr# addr 1# s1 of
        (# s2, a2 #) -> (# s2, FloatX2# a1 a2 #)
    {-# INLINE readAddr #-}

    writeAddr (FloatX2# a1 a2) addr s
      = writeFloatOffAddr# addr 1# a2
      ( writeFloatOffAddr# addr 0# a1 s )
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
      = FloatX2#
      (indexFloatArray# ba i)
      (indexFloatArray# ba (i +# 1#))
    {-# INLINE indexArray #-}

    readArray mba off s0
      | i <- off *# ELEM_N#
      = case readFloatArray# mba i s0 of
      (# s1, a1 #) -> case readFloatArray# mba (i +# 1#) s1 of
        (# s2, a2 #) -> (# s2, FloatX2# a1 a2 #)
    {-# INLINE readArray #-}

    writeArray mba off (FloatX2# a1 a2) s
      | i <- off *# ELEM_N#
      = writeFloatArray# mba (i +# 1#) a2
      ( writeFloatArray# mba  i        a1 s )
    {-# INLINE writeArray #-}


instance PrimArray Float FloatX2 where

    broadcast# (F# x) = FloatX2# x x
    {-# INLINE broadcast# #-}

    ix# 0# (FloatX2# a1 _) = F# a1
    ix# 1# (FloatX2# _ a2) = F# a2
    ix# _   _              = undefined
    {-# INLINE ix# #-}

    gen# _ f s0 = case f s0 of
      (# s1, F# a1 #) -> case f s1 of
        (# s2, F# a2 #) -> (# s2, FloatX2# a1 a2 #)


    upd# _ 0# (F# q) (FloatX2# _ y) = FloatX2# q y
    upd# _ 1# (F# q) (FloatX2# x _) = FloatX2# x q
    upd# _ _ _ x                    = x
    {-# INLINE upd# #-}

    withArrayContent# _ g x = g (CumulDims [ELEM_N, 1]) 0# (getBytes x)
    {-# INLINE withArrayContent# #-}

    offsetElems _ = 0#
    {-# INLINE offsetElems #-}

    uniqueOrCumulDims _ = Right (CumulDims [ELEM_N, 1])
    {-# INLINE uniqueOrCumulDims #-}

    fromElems# _ off ba = FloatX2#
      (indexFloatArray# ba off)
      (indexFloatArray# ba (off +# 1#))
    {-# INLINE fromElems# #-}
