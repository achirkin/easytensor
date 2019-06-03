{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}

module Numeric.DataFrame.Internal.Backend.Family.ArrayBase
  ( ArrayBase (..)
  ) where

import           Data.Coerce
import           Data.Int
import           Data.Word
import           GHC.Base                                          hiding
                                                                    (foldr)
import           Numeric.DataFrame.Internal.Backend.Family.PrimOps
import           Numeric.DataFrame.Internal.PrimArray
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.ProductOrd
import qualified Numeric.ProductOrd.NonTransitive                  as NonTransitive
import qualified Numeric.ProductOrd.Partial                        as Partial

-- | Generic Array implementation.
--   This array can reside in plain `ByteArray#` and can share the @ByteArray#@
--   with other arrays.
--   However, byte offset in the @ByteArray#@ must be multiple of the element size.
data ArrayBase (t :: Type) (ds :: [Nat])
  = ArrayBase
    (# t --  Same value for each element;
         --  this is the cheapest way to initialize an array.
         --  It is also used for Num instances to avoid dependency on Dimensions.
     | (# Int#  -- Offset in Content array (measured in elements)
        , ByteArray# -- Content;
                     -- elements are stored row-by-row, plane-by-plane;
                     -- (C-style, rather than Fortran-style);
                     -- similar to C, OpenCV, etc.
                     -- The data must be contiguous!
                     -- (i.e. stride must be the same size as the element size).
        , CumulDims  -- Steps array; [Word]
                     -- similar to steps in OpenCV::Mat, but counted in elements
                     --        rather than in bytes.
                     -- e.g. 0th element is the size of the content in elements
                     --      (data must be contiguous)
                     --      1th element is the size of subspace data minus outer dim.
        , Dict (PrimBytes t)
        #)
     #)


instance (PrimBytes t, Dimensions ds) => PrimBytes (ArrayBase t ds) where
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance Dimensions ds => PrimBytes (ArrayBase Word64 ds) #-}

    getBytes (ArrayBase a ) = case a of
        (# t | #)
          | W# nw <- totalDim' @ds
          , n <- word2Int# nw
          , tbs <- byteSize t -> go tbs (tbs *# n) t
        (# | (# _, arr, _, _ #) #) ->
          -- very weird trick with touch# allows to workaround GHC bug
          --  "internal error: ARR_WORDS object entered!"
          -- TODO: report this
          case runRW# (\s -> (# touch# arr s, arr #)) of (# _, ba #) -> ba
      where
        go :: Int# -> Int# -> t -> ByteArray#
        go tbs bsize t = case runRW#
         ( \s0 -> case newByteArray# bsize s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               ( loop# 0# tbs bsize (\i -> writeBytes mba i t) s1 )
         ) of (# _, ba #) -> ba
        {-# NOINLINE go #-}
    {-# INLINE getBytes #-}

    fromBytes bOff ba
      | tbs <- byteSize (undefined :: t)
      , (# offN, offRem #) <- quotRemInt# bOff tbs
      = case offRem of
          0# -> ArrayBase (# | (#  offN, ba, steps, Dict #) #)
          _  -> case cdTotalDim# steps of n -> go (tbs *# n)
      where
        steps = cumulDims $ dims @ds
        go bsize = case runRW#
         ( \s0 -> case ( if isTrue# (isByteArrayPinned# ba)
                         then newAlignedPinnedByteArray# bsize
                                (byteAlign @t undefined)
                         else newByteArray# bsize
                       ) s0
                  of
            (# s1, mba #) -> unsafeFreezeByteArray# mba
                              (copyByteArray# ba bOff mba 0# bsize s1)
         ) of (# _, r #) -> ArrayBase (# | (# 0#, r, steps, Dict #) #)
        {-# NOINLINE go #-}
    {-# INLINE fromBytes #-}

    readBytes mba bOff s0
      | steps <- cumulDims $ dims @ds
      , n <- cdTotalDim# steps
      , tbs <- byteSize (undefined :: t)
      , bsize <- tbs *# n
      = case newByteArray# bsize s0 of
         (# s1, mba1 #) -> case unsafeFreezeByteArray# mba1
                                (copyMutableByteArray# mba bOff mba1 0# bsize s1) of
           (# s2, ba #) -> (# s2, ArrayBase (# | (# 0#, ba, steps, Dict #) #) #)
    {-# INLINE readBytes #-}

    writeBytes mba bOff (ArrayBase c)
      | tbs <- byteSize (undefined :: t) = case c of
        (# t | #) | W# n <- totalDim' @ds ->
          loop# bOff tbs (bOff +# word2Int# n *# tbs) (\i -> writeBytes mba i t)
        (# | (# offContent, arrContent, steps, _ #) #)
           | n <- cdTotalDim# steps->
          copyByteArray# arrContent (offContent *# tbs) mba bOff (n *# tbs)
    {-# INLINE writeBytes #-}

    readAddr addr s0
      | steps <- cumulDims $ dims @ds
      , n <- cdTotalDim# steps
      , tbs <- byteSize (undefined :: t)
      , bsize <- tbs *# n
      = case newByteArray# bsize s0 of
         (# s1, mba1 #) -> case unsafeFreezeByteArray# mba1
                                (copyAddrToByteArray# addr mba1 0# bsize s1) of
           (# s2, ba #) -> (# s2, ArrayBase (# | (# 0# , ba, steps, Dict #) #) #)
    {-# INLINE readAddr #-}

    writeAddr (ArrayBase c) addr
      | tbs <- byteSize (undefined :: t) = case c of
        (# t | #) | W# n <- totalDim' @ds ->
          loop# 0# tbs (word2Int# n *# tbs) (\i -> writeAddr t (plusAddr# addr i))
        (# | (# offContent, arrContent, steps, _ #) #)
           | n <- cdTotalDim# steps ->
          copyByteArrayToAddr# arrContent (offContent *# tbs) addr (n *# tbs)
    {-# INLINE writeAddr #-}


    byteSize  _ = case totalDim' @ds of -- WARNING: slow!
      W# n -> byteSize (undefined :: t) *# word2Int# n
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign (undefined :: t)
    {-# INLINE byteAlign #-}

    byteOffset (ArrayBase a) = case a of
      (# _ | #)                  -> 0#
      (# | (# off, _, _, _ #) #) -> off *# byteSize (undefined :: t)
    {-# INLINE byteOffset #-}

    indexArray ba off
      | steps <- cumulDims $ dims @ds
      , n <- cdTotalDim# steps
      = ArrayBase (# | (# off *# n, ba, steps, Dict #) #)
    {-# INLINE indexArray #-}



-- | Accumulates only idempotent operations!
--   Being applied to FromScalars, executes only once!
--   Here, idempotance means: assuming @f a b = g x@, @g (g x) = g x@
--
--   Also, I assume the sizes of arrays are the same.
--
--   Inside, this function uses foldr; thus, if the combining function is
--   lazy in the second argument, it may avoid some unnecessary work.
accumV2Idempotent :: a
                  -> (t -> t -> a)
                  -> (a -> a -> a)
                  -> ArrayBase t ds -> ArrayBase t ds -> a
accumV2Idempotent x f comb
  (ArrayBase (# a | #))
  (ArrayBase (# b | #))
    = comb x (f a b)
accumV2Idempotent x f comb
  a@(ArrayBase (# | (# _, _, steps, Dict #) #))
  b@(ArrayBase (# | _ #))
    = foldr (comb . (\i -> f (ixOff i a) (ixOff i b))) x
                          [0 .. fromIntegral (cdTotalDim steps) - 1]
accumV2Idempotent x f comb
    (ArrayBase (# a | #))
  b@(ArrayBase (# | (# _, _, steps, Dict #) #))
    = foldr (comb . (\i -> f a (ixOff i b))) x
                          [0 .. fromIntegral (cdTotalDim steps) - 1]
accumV2Idempotent x f comb
  a@(ArrayBase (# | (# _, _, steps, Dict #) #))
    (ArrayBase (# b | #))
    = foldr (comb . (\i -> f (ixOff i a) b)) x
                          [0 .. fromIntegral (cdTotalDim steps) - 1]
{-# INLINE accumV2Idempotent #-}


mapV :: (t -> t) -> ArrayBase t ds -> ArrayBase t ds
mapV f (ArrayBase (# t | #))
    = ArrayBase (# f t | #)
mapV f x@(ArrayBase (# | (# offN, ba, steps, Dict #) #))
    | tbs <- byteSize (undefEl x)
    , n <- cdTotalDim# steps
    = case runRW#
     ( \s0 -> case newByteArray# (tbs *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i (f (indexArray ba (offN +# i)))) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, r, steps, Dict #) #)
{-# INLINE mapV #-}


zipV :: (t -> t -> t)
     -> ArrayBase t ds -> ArrayBase t ds -> ArrayBase t ds
zipV f (ArrayBase (# x | #)) b = mapV (f x) b
zipV f a (ArrayBase (# y | #)) = mapV (`f` y) a
zipV f a@(ArrayBase (# | (# oa, ba, steps, Dict #) #))
         (ArrayBase (# | (# ob, bb, _, _ #) #))
    | n <- cdTotalDim# steps
    = case runRW#
     ( \s0 -> case newByteArray# (byteSize (undefEl a) *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i
                        (f (indexArray ba (oa +# i))
                           (indexArray bb (ob +# i))
                        )
               ) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, r, steps, Dict #) #)
{-# INLINE zipV #-}


-- TODO: to improve performance, I can either compare bytearrays using memcmp
--       or implement early termination if the first elements do not match.
--       On the other hand, hopefully @(&&)@ and @(||)@ ops take care of that.
instance Eq t => Eq (ArrayBase t ds) where
    {-# SPECIALIZE instance Eq (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Eq (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance Eq (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance Eq (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance Eq (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance Eq (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance Eq (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance Eq (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance Eq (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance Eq (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance Eq (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance Eq (ArrayBase Word64 ds) #-}
    (==) = accumV2Idempotent True (==) (&&)
    (/=) = accumV2Idempotent False (/=) (||)

instance Ord t => ProductOrder (ArrayBase t ds) where
    {-# SPECIALIZE instance ProductOrder (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance ProductOrder (ArrayBase Word64 ds) #-}
    cmp = accumV2Idempotent PEQ (\x y -> fromOrdering (compare x y)) (<>)
    {-# INLINE cmp #-}

instance Ord t => Ord (NonTransitive.ProductOrd (ArrayBase t ds)) where
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Float ds))  #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Double ds)) #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Int ds))    #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Word ds))   #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Int8 ds))   #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Int16 ds))  #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Int32 ds))  #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Int64 ds))  #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Word8 ds))  #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Word16 ds)) #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Word32 ds)) #-}
    {-# SPECIALIZE instance Ord (NonTransitive.ProductOrd (ArrayBase Word64 ds)) #-}
    NonTransitive.ProductOrd x > NonTransitive.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    NonTransitive.ProductOrd x < NonTransitive.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) = coerce (accumV2Idempotent True (>=) (&&))
    {-# INLINE (>=) #-}
    (<=) = coerce (accumV2Idempotent True (<=) (&&))
    {-# INLINE (<=) #-}
    compare (NonTransitive.ProductOrd a) (NonTransitive.ProductOrd b)
      = NonTransitive.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min = coerce (zipV min)
    {-# INLINE min #-}
    max = coerce (zipV max)
    {-# INLINE max #-}

instance Ord t => Ord (Partial.ProductOrd (ArrayBase t ds)) where
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Float ds))  #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Double ds)) #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Int ds))    #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Word ds))   #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Int8 ds))   #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Int16 ds))  #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Int32 ds))  #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Int64 ds))  #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Word8 ds))  #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Word16 ds)) #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Word32 ds)) #-}
    {-# SPECIALIZE instance Ord (Partial.ProductOrd (ArrayBase Word64 ds)) #-}
    Partial.ProductOrd x > Partial.ProductOrd y = cmp x y == PGT
    {-# INLINE (>) #-}
    Partial.ProductOrd x < Partial.ProductOrd y = cmp x y == PLT
    {-# INLINE (<) #-}
    (>=) = coerce (accumV2Idempotent True (>=) (&&))
    {-# INLINE (>=) #-}
    (<=) = coerce (accumV2Idempotent True (<=) (&&))
    {-# INLINE (<=) #-}
    compare (Partial.ProductOrd a) (Partial.ProductOrd b)
      = Partial.toOrdering $ cmp a b
    {-# INLINE compare #-}
    min = coerce (zipV min)
    {-# INLINE min #-}
    max = coerce (zipV max)
    {-# INLINE max #-}

-- | Lexicographical ordering
instance Ord t => Ord (ArrayBase t ds) where
    {-# SPECIALIZE instance Ord (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Ord (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance Ord (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance Ord (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance Ord (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance Ord (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance Ord (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance Ord (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance Ord (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance Ord (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance Ord (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance Ord (ArrayBase Word64 ds) #-}
    compare = accumV2Idempotent EQ compare (<>)
    {-# INLINE compare #-}


instance {-# OVERLAPPING #-} Bounded (ArrayBase Double ds) where
    maxBound = ArrayBase (# inftyD | #)
    minBound = ArrayBase (# negate inftyD | #)

instance {-# OVERLAPPING #-} Bounded (ArrayBase Float ds) where
    maxBound = ArrayBase (# inftyF | #)
    minBound = ArrayBase (# negate inftyF | #)

instance {-# INCOHERENT #-} Bounded t => Bounded (ArrayBase t ds) where
    {-# SPECIALIZE instance Bounded (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance Bounded (ArrayBase Word64 ds) #-}
    maxBound = ArrayBase (# maxBound | #)
    minBound = ArrayBase (# minBound | #)

instance Num t => Num (ArrayBase t ds)  where
    {-# SPECIALIZE instance Num (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Num (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance Num (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance Num (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance Num (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance Num (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance Num (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance Num (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance Num (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance Num (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance Num (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance Num (ArrayBase Word64 ds) #-}
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
    fromInteger i = ArrayBase (# fromInteger i | #)
    {-# INLINE fromInteger #-}

instance Fractional t => Fractional (ArrayBase t ds)  where
    {-# SPECIALIZE instance Fractional (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Fractional (ArrayBase Double ds) #-}
    (/) = zipV (/)
    {-# INLINE (/) #-}
    recip = mapV recip
    {-# INLINE recip #-}
    fromRational r = ArrayBase (# fromRational r | #)
    {-# INLINE fromRational #-}


instance Floating t => Floating (ArrayBase t ds) where
    {-# SPECIALIZE instance Floating (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance Floating (ArrayBase Double ds) #-}
    pi = ArrayBase (# pi | #)
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

instance PrimBytes t => PrimArray t (ArrayBase t ds) where
    {-# SPECIALIZE instance PrimArray Float  (ArrayBase Float ds)  #-}
    {-# SPECIALIZE instance PrimArray Double (ArrayBase Double ds) #-}
    {-# SPECIALIZE instance PrimArray Int    (ArrayBase Int ds)    #-}
    {-# SPECIALIZE instance PrimArray Word   (ArrayBase Word ds)   #-}
    {-# SPECIALIZE instance PrimArray Int8   (ArrayBase Int8 ds)   #-}
    {-# SPECIALIZE instance PrimArray Int16  (ArrayBase Int16 ds)  #-}
    {-# SPECIALIZE instance PrimArray Int32  (ArrayBase Int32 ds)  #-}
    {-# SPECIALIZE instance PrimArray Int64  (ArrayBase Int64 ds)  #-}
    {-# SPECIALIZE instance PrimArray Word8  (ArrayBase Word8 ds)  #-}
    {-# SPECIALIZE instance PrimArray Word16 (ArrayBase Word16 ds) #-}
    {-# SPECIALIZE instance PrimArray Word32 (ArrayBase Word32 ds) #-}
    {-# SPECIALIZE instance PrimArray Word64 (ArrayBase Word64 ds) #-}

    broadcast t = ArrayBase (# t | #)
    {-# INLINE broadcast #-}

    ix# i (ArrayBase a) = case a of
      (# t | #)                    -> t
      (# | (# off, arr, _, _ #) #) -> indexArray arr (off +# i)
    {-# INLINE ix# #-}

    gen# steps f z0 = go (byteSize @t undefined *# n)
      where
        n = cdTotalDim# steps
        go bsize = case runRW#
         ( \s0 -> case newByteArray# bsize s0 of
             (# s1, mba #) -> case loop0 mba 0# z0 s1 of
               (# s2, z1 #) -> case unsafeFreezeByteArray# mba s2 of
                 (# s3, ba #) -> (# s3, (# z1, ba #) #)
         ) of (# _, (# z1, ba #) #)
                   -> (# z1, ArrayBase (# | (# 0# , ba, steps, Dict #) #) #)
        {-# NOINLINE go #-}
        loop0 mba i z s
          | isTrue# (i ==# n) = (# s, z #)
          | otherwise = case f z of
              (# z', x #) -> loop0 mba (i +# 1#) z' (writeArray mba i x s)
    {-# INLINE gen# #-}

    upd# steps i x (ArrayBase (# a | #)) = go (byteSize x)
      where
        n = cdTotalDim# steps
        go tbs = case runRW#
         ( \s0 -> case newByteArray# (tbs *# n) s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               (writeArray mba i x
                 (loop1# n (\j -> writeArray mba j a) s1)
               )
         ) of (# _, r #) -> ArrayBase (# |  (# 0# , r, steps, Dict #) #)
        {-# NOINLINE go #-}
    upd# _ i x (ArrayBase (# | (# offN, ba, steps, Dict #) #))
        = go (byteSize x)
      where
        n = cdTotalDim# steps
        go tbs = case runRW#
         ( \s0 -> case newByteArray# (tbs *# n) s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               (writeArray mba i x
                 (copyByteArray# ba (offN *# tbs) mba 0# (tbs *# n) s1)
               )
         ) of (# _, r #) -> ArrayBase (# | (# 0#, r, steps, Dict #) #)
        {-# NOINLINE go #-}
    {-# INLINE upd# #-}

    arrayContent# (ArrayBase a) = case a of
      (# x | #)                     -> (# x | #)
      (# | (# off, arr, cd, _ #) #) -> (# | (# cd, off, arr #) #)
    {-# INLINE arrayContent# #-}

    offsetElems (ArrayBase a) = case a of
      (# _ | #)                  -> 0#
      (# | (# off, _, _, _ #) #) -> off
    {-# INLINE offsetElems #-}

    uniqueOrCumulDims (ArrayBase a) = case a of
      (# t | #)                    -> Left t
      (# | (# _, _, steps, _ #) #) -> Right steps
    {-# INLINE uniqueOrCumulDims #-}

    fromElems steps off ba = ArrayBase (# | (# off, ba, steps, Dict #) #)
    {-# INLINE fromElems #-}


undefEl :: ArrayBase t ds -> t
undefEl = const undefined
