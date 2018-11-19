{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedSums                #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Numeric.DataFrame.Internal.Array.Family.ArrayBase
  ( ArrayBase (..)
  ) where

import           GHC.Base                                        hiding (foldr)
import {-# SOURCE #-} Numeric.DataFrame.Internal.Array.Family (Array)
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.Dimensions
import           Numeric.PrimBytes
import           Numeric.DataFrame.Family

-- | Generic Array implementation.
--   This array can reside in plain `ByteArray#` and can share the @ByteArray#@
--   with other arrays.
--   However, byte offset in the @ByteArray#@ must be multiple of the element size.
data ArrayBase (t :: Type) (ds :: [Nat])
  = ArrayBase
    (# t
       --  Same value for each element;
       --  this is the cheapest way to initialize an array.
       --  It is also used for Num instances to avoid dependency on Dimensions.
     | (# Int#  -- Offset measured in elements.
        , Int#  -- Number of elements.
        , ByteArray# -- Content.
        #)
     #)


instance {-# OVERLAPPABLE #-}
  ( Array t ds ~ ArrayBase t ds
  , PrimBytes t, Dimensions ds
  ) => PrimBytes (DataFrame (t :: Type) (ds :: [Nat])) where
    getBytes (SingleFrame (ArrayBase a)) = case a of
        (# t | #)
          | W# nw <- totalDim' @ds
          , n <- word2Int# nw
          , tbs <- byteSize t   -> go tbs (tbs *# n) t
        (# | (# _, _, arr #) #) ->
          -- very weird trick with touch# allows to workaround GHC bug
          --  "internal error: ARR_WORDS object entered!"
          -- TODO: report this
          case runRW# (\s -> (# touch# arr s, arr #)) of (# _, ba #) -> ba
      where
        go tbs bsize t = case runRW#
         ( \s0 -> case newByteArray# bsize s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               ( loop# 0# tbs bsize (\i -> writeBytes mba i t) s1 )
         ) of (# _, ba #) -> ba
        {-# NOINLINE go #-}
    {-# INLINE getBytes #-}

    fromBytes bOff ba
      | W# nw <- totalDim' @ds
      , n <- word2Int# nw
      , tbs <- byteSize (undefined :: t)
      , (# offN, offRem #) <- quotRemInt# bOff tbs
      = case offRem of
          0# -> SingleFrame (ArrayBase (# | (# offN, n , ba #) #))
          _  -> go n (tbs *# n)
      where
        go n bsize = case runRW#
         ( \s0 -> case ( if isTrue# (isByteArrayPinned# ba)
                         then newAlignedPinnedByteArray# bsize
                                (byteAlign @t undefined)
                         else newByteArray# bsize
                       ) s0
                  of
            (# s1, mba #) -> unsafeFreezeByteArray# mba
                              (copyByteArray# ba bOff mba 0# bsize s1)
         ) of (# _, r #) -> SingleFrame (ArrayBase (# | (# 0# , n , r #) #))
        {-# NOINLINE go #-}
    {-# INLINE fromBytes #-}

    readBytes mba bOff s0
      | W# nw <- totalDim' @ds
      , n <- word2Int# nw
      , tbs <- byteSize (undefined :: t)
      , bsize <- tbs *# n
      = case newByteArray# bsize s0 of
         (# s1, mba1 #) -> case unsafeFreezeByteArray# mba1
                                (copyMutableByteArray# mba bOff mba1 0# bsize s1) of
           (# s2, ba #) -> (# s2, SingleFrame (ArrayBase (# | (# 0# , n , ba #) #)) #)
    {-# INLINE readBytes #-}

    writeBytes mba bOff (SingleFrame (ArrayBase c))
      | tbs <- byteSize (undefined :: t) = case c of
        (# t | #) | W# n <- totalDim' @ds ->
          loop# bOff tbs (bOff +# word2Int# n *# tbs) (\i -> writeBytes mba i t)
        (# | (# offN, n, arr #) #) ->
          copyByteArray# arr (offN *# tbs) mba bOff (n *# tbs)
    {-# INLINE writeBytes #-}

    readAddr addr s0
      | W# nw <- totalDim' @ds
      , n <- word2Int# nw
      , tbs <- byteSize (undefined :: t)
      , bsize <- tbs *# n
      = case newByteArray# bsize s0 of
         (# s1, mba1 #) -> case unsafeFreezeByteArray# mba1
                                (copyAddrToByteArray# addr mba1 0# bsize s1) of
           (# s2, ba #) -> (# s2, SingleFrame (ArrayBase (# | (# 0# , n , ba #) #)) #)
    {-# INLINE readAddr #-}

    writeAddr (SingleFrame (ArrayBase c)) addr
      | tbs <- byteSize (undefined :: t) = case c of
        (# t | #) | W# n <- totalDim' @ds ->
          loop# 0# tbs (word2Int# n *# tbs) (\i -> writeAddr t (plusAddr# addr i))
        (# | (# offN, n, arr #) #) ->
          copyByteArrayToAddr# arr (offN *# tbs) addr (n *# tbs)
    {-# INLINE writeAddr #-}


    byteSize  _ = case totalDim' @ds of -- WARNING: slow!
      W# n -> byteSize (undefined :: t) *# word2Int# n
    {-# INLINE byteSize #-}

    byteAlign _ = byteAlign (undefined :: t)
    {-# INLINE byteAlign #-}

    byteOffset (SingleFrame (ArrayBase a)) = case a of
      (# _ | #)               -> 0#
      (# | (# off, _, _ #) #) -> off *# byteSize (undefined :: t)
    {-# INLINE byteOffset #-}

    indexArray ba off
      | W# nw <- totalDim' @ds
      , n <- word2Int# nw
      = SingleFrame (ArrayBase (# | (# off *# n, n, ba #) #))
    {-# INLINE indexArray #-}



-- | Accumulates only idempotent operations!
--   Being applied to FromScalars, executes only once!
--   Here, idempotance means: assuming @f a b = g @, @g (g x) = g x@
--
--   Also, I assume the size of arrays is the same
accumV2Idempotent :: ( Array t ds ~ ArrayBase t ds
                     , PrimBytes t )
                  => a
                  -> (t -> t -> a -> a)
                  -> ArrayBase t ds -> ArrayBase t ds -> a
accumV2Idempotent x f
  (ArrayBase (# a | #))
  (ArrayBase (# b | #))
    = f a b x
accumV2Idempotent x f
  a@(ArrayBase (# | (# _, nA, _ #) #))
  b@(ArrayBase (# | (# _, nB, _ #) #))
    = loop1a# (minInt# nA nB) (\i -> f (ix# i (SingleFrame a)) (ix# i (SingleFrame b))) x
accumV2Idempotent x f
    (ArrayBase (# a | #))
  b@(ArrayBase (# | (# _, n, _ #) #))
    = loop1a# n (\i -> f a (ix# i (SingleFrame b))) x
accumV2Idempotent x f
  a@(ArrayBase (# | (# _, n, _ #) #))
    (ArrayBase (# b | #))
    = loop1a# n (\i -> f (ix# i (SingleFrame a)) b) x
{-# INLINE accumV2Idempotent #-}

mapV :: PrimBytes t => (t -> t) -> ArrayBase t ds -> ArrayBase t ds
mapV f (ArrayBase (# t | #))
    = ArrayBase (# f t | #)
mapV f x@(ArrayBase (# | (# offN, n, ba #) #))
    | tbs <- byteSize (undefEl x)
    = go (tbs *# n)
  where
    go bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i (f (indexArray ba (offN +# i)))) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, n, r #) #)
    {-# NOINLINE go #-}
{-# INLINE mapV #-}


zipV :: PrimBytes t => (t -> t -> t)
     -> ArrayBase t ds -> ArrayBase t ds -> ArrayBase t ds
zipV f (ArrayBase (# x | #)) b = mapV (f x) b
zipV f a (ArrayBase (# y | #)) = mapV (flip f y) a
zipV f a@(ArrayBase (# | (# oa, na, ba #) #))
         (ArrayBase (# | (# ob, nb, bb #) #))
    | n <- (minInt# na nb)
    = go n (byteSize (undefEl a) *# n)
  where
    go n bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i
                        (f (indexArray ba (oa +# i))
                           (indexArray bb (ob +# i))
                        )
               ) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, n, r #) #)
    {-# NOINLINE go #-}
{-# INLINE zipV #-}


-- TODO: to improve performance, I can either compare bytearrays using memcmp
--       or implement early termination if the first elements do not match.
--       On the other hand, hopefully @(&&)@ and @(||)@ ops take care of that.
instance  {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , Eq t, PrimBytes t
         ) => Eq (DataFrame t ds) where
    (==) = unsafeCoerce# (accumV2Idempotent @t @ds True  (\x y r -> r && x == y))
    {-# INLINE (==) #-}
    (/=) = unsafeCoerce# (accumV2Idempotent @t @ds False (\x y r -> r || x /= y))
    {-# INLINE (/=) #-}


-- TODO: Split into two instances: lexicographical order vs product order
--       https://en.wikipedia.org/wiki/Product_order
--
-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--     and lexicographical ordering for `compare`
instance  {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , Ord t, PrimBytes t
         ) => Ord (DataFrame t ds)  where
    -- | Partiall ordering: all elements GT
    (>)  = unsafeCoerce# (accumV2Idempotent @t @ds True (\x y r -> r && x > y))
    {-# INLINE (>) #-}
    -- | Partiall ordering: all elements LT
    (<)  = unsafeCoerce# (accumV2Idempotent @t @ds True (\x y r -> r && x < y))
    {-# INLINE (<) #-}
    -- | Partiall ordering: all elements GE
    (>=) = unsafeCoerce# (accumV2Idempotent @t @ds True (\x y r -> r && x >= y))
    {-# INLINE (>=) #-}
    -- | Partiall ordering: all elements LE
    (<=) = unsafeCoerce# (accumV2Idempotent @t @ds True (\x y r -> r && x <= y))
    {-# INLINE (<=) #-}
    -- | Compare lexicographically
    compare = unsafeCoerce# (accumV2Idempotent @t @ds EQ (\x y  -> flip mappend (compare x y)))
    {-# INLINE compare #-}
    -- | Element-wise minimum
    min = unsafeCoerce# (zipV @t @ds min)
    {-# INLINE min #-}
    -- | Element-wise maximum
    max = unsafeCoerce# (zipV @t @ds max)
    {-# INLINE max #-}

-- TODO: make a proper ugly fromList Show instance
-- instance (Dimensions ds, PrimBytes t, Show t)
--       => Show (ArrayBase t ds) where
--   show x = case dims @_ @ds of
--     U -> "{ " ++ show (ix# 0# x) ++ " }"
--     Dim :* U -> ('{' :) . drop 1 $
--                     foldr (\i s -> ", " ++ show (ix i x) ++ s) " }"
--                             [minBound .. maxBound]
--     (Dim :: Dim n) :* (Dim :: Dim m) :* (Dims :: Dims dss) ->
--       let loopInner :: Idxs dss -> Idxs '[n,m] -> String
--           loopInner ods (n:*m:*_) = ('{' :) . drop 2 $
--                           foldr (\i ss -> '\n':
--                                   foldr (\j s ->
--                                            ", " ++ show (ix (i :* j :* ods) x) ++ s
--                                         ) ss [1..m]
--                                 ) " }" [1..n]
--           loopOuter ::  Idxs dss -> String -> String
--           loopOuter U s  = "\n" ++ loopInner U maxBound ++ s
--           loopOuter ds s = "\n(i j" ++ drop 4 (show ds) ++ "):\n"
--                                 ++ loopInner ds maxBound ++ s
--       in drop 1 $ foldr loopOuter "" [minBound..maxBound]

instance {-# OVERLAPS #-}
      ( Array Double ds ~ ArrayBase Double ds ) => Bounded (DataFrame Double ds) where
    maxBound = unsafeCoerce# (ArrayBase (# inftyD | #))
    minBound = unsafeCoerce# (ArrayBase (# negate inftyD | #))

instance {-# OVERLAPS #-}
      ( Array Float ds ~ ArrayBase Float ds ) => Bounded (DataFrame Float ds) where
    maxBound = unsafeCoerce# (ArrayBase (# inftyF | #))
    minBound = unsafeCoerce# (ArrayBase (# negate inftyF | #))

instance {-# OVERLAPPABLE #-}
      ( Array t ds ~ ArrayBase t ds
      , Bounded t ) => Bounded (DataFrame t ds) where
    maxBound = unsafeCoerce# (ArrayBase (# maxBound | #) :: Array t ds)
    minBound = unsafeCoerce# (ArrayBase (# minBound | #) :: Array t ds)

instance {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , Num t, PrimBytes t
         ) => Num (DataFrame t ds)  where
    (+) = unsafeCoerce# (zipV @t @ds (+))
    {-# INLINE (+) #-}
    (-) = unsafeCoerce# (zipV @t @ds (-))
    {-# INLINE (-) #-}
    (*) = unsafeCoerce# (zipV @t @ds (*))
    {-# INLINE (*) #-}
    negate = unsafeCoerce# (mapV @t @ds negate)
    {-# INLINE negate #-}
    abs = unsafeCoerce# (mapV @t @ds abs)
    {-# INLINE abs #-}
    signum = unsafeCoerce# (mapV @t @ds signum)
    {-# INLINE signum #-}
    fromInteger i = unsafeCoerce# (ArrayBase (# fromInteger i | #) :: Array t ds)
    {-# INLINE fromInteger #-}

instance {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , Fractional t, PrimBytes t
         ) => Fractional (DataFrame t ds)  where
    (/) = unsafeCoerce# (zipV @t @ds (/))
    {-# INLINE (/) #-}
    recip = unsafeCoerce# (mapV @t @ds recip)
    {-# INLINE recip #-}
    fromRational r = unsafeCoerce# (ArrayBase (# fromRational r | #) :: Array t ds)
    {-# INLINE fromRational #-}


instance {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , Floating t, PrimBytes t
         ) => Floating (DataFrame t ds) where
    pi = unsafeCoerce# (ArrayBase (# pi | #) :: Array t ds)
    {-# INLINE pi #-}
    exp = unsafeCoerce# (mapV @t @ds exp)
    {-# INLINE exp #-}
    log = unsafeCoerce# (mapV @t @ds log)
    {-# INLINE log #-}
    sqrt = unsafeCoerce# (mapV @t @ds sqrt)
    {-# INLINE sqrt #-}
    sin = unsafeCoerce# (mapV @t @ds sin)
    {-# INLINE sin #-}
    cos = unsafeCoerce# (mapV @t @ds cos)
    {-# INLINE cos #-}
    tan = unsafeCoerce# (mapV @t @ds tan)
    {-# INLINE tan #-}
    asin = unsafeCoerce# (mapV @t @ds asin)
    {-# INLINE asin #-}
    acos = unsafeCoerce# (mapV @t @ds acos)
    {-# INLINE acos #-}
    atan = unsafeCoerce# (mapV @t @ds atan)
    {-# INLINE atan #-}
    sinh = unsafeCoerce# (mapV @t @ds sinh)
    {-# INLINE sinh #-}
    cosh = unsafeCoerce# (mapV @t @ds cosh)
    {-# INLINE cosh #-}
    tanh = unsafeCoerce# (mapV @t @ds tanh)
    {-# INLINE tanh #-}
    (**) = unsafeCoerce# (zipV @t @ds (**))
    {-# INLINE (**) #-}
    logBase = unsafeCoerce# (zipV @t @ds logBase)
    {-# INLINE logBase #-}
    asinh = unsafeCoerce# (mapV @t @ds asinh)
    {-# INLINE asinh #-}
    acosh = unsafeCoerce# (mapV @t @ds acosh)
    {-# INLINE acosh #-}
    atanh = unsafeCoerce# (mapV @t @ds atanh)
    {-# INLINE atanh #-}

instance {-# OVERLAPPABLE #-}
         ( Array t ds ~ ArrayBase t ds
         , PrimBytes t
         ) => PrimArray t (DataFrame t ds) where

    broadcast t = unsafeCoerce# (ArrayBase (# t | #) :: Array t ds)
    {-# INLINE broadcast #-}

    ix# i (SingleFrame (ArrayBase a)) = case a of
      (# t | #)                 -> t
      (# | (# off, _, arr #) #) -> indexArray arr (off +# i)
    {-# INLINE ix# #-}

    gen# n f z0 = go (byteSize @t undefined *# n)
      where
        go bsize = case runRW#
         ( \s0 -> case newByteArray# bsize s0 of
             (# s1, mba #) -> case loop0 mba 0# z0 s1 of
               (# s2, z1 #) -> case unsafeFreezeByteArray# mba s2 of
                 (# s3, ba #) -> (# s3, (# z1, ba #) #)
         ) of (# _, (# z1, ba #) #) -> (# z1, unsafeCoerce# (ArrayBase (# | (# 0# , n , ba #) #) :: Array t ds) #)
        {-# NOINLINE go #-}
        loop0 mba i z s
          | isTrue# (i ==# n) = (# s, z #)
          | otherwise = case f z of
              (# z', x #) -> loop0 mba (i +# 1#) z' (writeArray mba i x s)
    {-# INLINE gen# #-}

    upd# n i x (SingleFrame (ArrayBase (# a | #))) = go (byteSize x)
      where
        go tbs = case runRW#
         ( \s0 -> case newByteArray# (tbs *# n) s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               (writeArray mba i x
                 (loop1# n (\j -> writeArray mba j a) s1)
               )
         ) of (# _, r #) -> unsafeCoerce# (ArrayBase (# | (# 0# , n , r #) #) :: Array t ds)
        {-# NOINLINE go #-}
    upd# _ i x (SingleFrame (ArrayBase (# | (# offN , n , ba #) #))) = go (byteSize x)
      where
        go tbs = case runRW#
         ( \s0 -> case newByteArray# (tbs *# n) s0 of
             (# s1, mba #) -> unsafeFreezeByteArray# mba
               (writeArray mba i x
                 (copyByteArray# ba (offN *# tbs) mba 0# (tbs *# n) s1)
               )
         ) of (# _, r #) -> unsafeCoerce# (ArrayBase (# | (# 0# , n , r #) #) :: Array t ds)
        {-# NOINLINE go #-}
    {-# INLINE upd# #-}

    elemOffset (SingleFrame (ArrayBase a)) = case a of
      (# _ | #)               -> 0#
      (# | (# off, _, _ #) #) -> off
    {-# INLINE elemOffset #-}

    elemSize0 (SingleFrame (ArrayBase a)) = case a of
      (# _ | #)             -> 0#
      (# | (# _, n, _ #) #) -> n
    {-# INLINE elemSize0 #-}

    fromElems off n ba = unsafeCoerce# (ArrayBase (# | (# off , n , ba #) #) :: Array t ds)
    {-# INLINE fromElems #-}



--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------


-- ix :: (PrimBytes t, Dimensions ds) => Idxs ds -> ArrayBase t ds -> t
-- ix i (ArrayBase a) = case a of
--   (# t | #)  -> t
--   (# | (# off, _, arr #) #) -> case fromEnum i of
--     I# i# -> indexArray arr (off +# i#)
-- {-# INLINE ix #-}


undefEl :: ArrayBase t ds -> t
undefEl = const undefined
