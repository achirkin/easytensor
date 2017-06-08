{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UnboxedTuples          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.Contraction
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- This modules provides generalization of a matrix product:
--  tensor-like contraction.
-- For matrices and vectors this is a normal matrix*matrix or vector*matrix or matrix*vector product,
-- for larger dimensions it calculates the scalar product of "adjacent" dimesnions of a tensor.
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.Contraction
  ( Contraction (..), (%*)
  ) where

import GHC.Types (Type, Int (..), Word (..), RuntimeRep (..), isTrue#)
import GHC.TypeLits (Nat, KnownNat, natVal)
import GHC.Base (runRW#)
import GHC.Prim
import Data.Proxy
import Data.Int
import Data.Word
import Data.Type.Equality
import Unsafe.Coerce

import Numeric.Array.Family
import Numeric.Commons
import Numeric.Dimensions
import Numeric.DataFrame.Type


class ConcatList as bs asbs
      => Contraction (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                             | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Generalization of a matrix product: take scalar product over one dimension
    --   and, thus, concatenate other dimesnions
    contract :: ( KnownNat m
                , PrimBytes (DataFrame t (as +: m))
                , PrimBytes (DataFrame t (m :+ bs))
                , PrimBytes (DataFrame t asbs)
                )
             => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.
(%*) :: ( ConcatList (as +: m) (m ': bs) (as ++ bs)
        , Contraction t as bs asbs
        , KnownNat m
        , PrimBytes (DataFrame t (as +: m))
        , PrimBytes (DataFrame t (m :+ bs))
        , PrimBytes (DataFrame t (as ++ bs))
        )  => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t (as ++ bs)
(%*) = contract
{-# INLINE (%*) #-}
infixl 7 %*


--------------------------------------------------------------------------------

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Float as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Float (m : bs) ) :~: 'FloatRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Float (m : bs) ) :~:  Float#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Float (as +: m)) :~: 'FloatRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Float (as +: m)) :~:  Float#
        = prodF n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy


instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Double as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Double (m : bs) ) :~: 'DoubleRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Double (m : bs) ) :~:  Double#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Double (as +: m)) :~: 'DoubleRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Double (as +: m)) :~:  Double#
        = prodD n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Int as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int (m : bs) ) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int (m : bs) ) :~:  Int#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int (as +: m)) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int (as +: m)) :~:  Int#
        = prodI n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Int8 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int8 (m : bs) ) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int8 (m : bs) ) :~:  Int#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int8 (as +: m)) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int8 (as +: m)) :~:  Int#
        = prodI8 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Int16 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int16 (m : bs) ) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int16 (m : bs) ) :~:  Int#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int16 (as +: m)) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int16 (as +: m)) :~:  Int#
        = prodI16 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Int32 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int32 (m : bs) ) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int32 (m : bs) ) :~:  Int#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int32 (as +: m)) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int32 (as +: m)) :~:  Int#
        = prodI32 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Int64 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int64 (m : bs) ) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int64 (m : bs) ) :~:  Int#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Int64 (as +: m)) :~: 'IntRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Int64 (as +: m)) :~:  Int#
        = prodI64 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy




instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Word as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word (m : bs) ) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word (m : bs) ) :~:  Word#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word (as +: m)) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word (as +: m)) :~:  Word#
        = prodW n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Word8 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word8 (m : bs) ) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word8 (m : bs) ) :~:  Word#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word8 (as +: m)) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word8 (as +: m)) :~:  Word#
        = prodW8 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Word16 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word16 (m : bs) ) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word16 (m : bs) ) :~:  Word#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word16 (as +: m)) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word16 (as +: m)) :~:  Word#
        = prodW16 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Word32 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word32 (m : bs) ) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word32 (m : bs) ) :~:  Word#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word32 (as +: m)) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word32 (as +: m)) :~:  Word#
        = prodW32 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy

instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         ) => Contraction Word64 as bs asbs where
    contract x y
        | (pm :: Proxy m) <- getM y
        , I# m <- fromInteger $ natVal pm
        , I# n <- totalDim (Proxy @as)
        , I# k <- totalDim (Proxy @bs)
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word64 (m : bs) ) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word64 (m : bs) ) :~:  Word#
        , Refl <- unsafeCoerce Refl :: ElemRep  (Array Word64 (as +: m)) :~: 'WordRep
        , Refl <- unsafeCoerce Refl :: ElemPrim (Array Word64 (as +: m)) :~:  Word#
        = prodW64 n m k x y
      where
        getM :: forall m p . p (m ': bs) -> Proxy m
        getM _ = Proxy





prodF :: (FloatBytes a, FloatBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodF n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusFloat#` timesFloat# (ix (i +# n *# l) x)
                                                                                           (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeFloatArray# marr (i +# n *# j) (loop' i j 0# 0.0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodF #-}

prodD :: (DoubleBytes a, DoubleBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodD n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +## (*##) (ix (i +# n *# l) x)
                                                                            (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeDoubleArray# marr (i +# n *# j) (loop' i j 0# 0.0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodD #-}

prodI :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ix (i +# n *# l) x)
                                                                          (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeIntArray# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodI #-}

prodI8 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI8 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ix (i +# n *# l) x)
                                                                          (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt8Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodI8 #-}


prodI16 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI16 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ix (i +# n *# l) x)
                                                                          (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt16Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodI16 #-}


prodI32 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI32 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ix (i +# n *# l) x)
                                                                          (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt32Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodI32 #-}


prodI64 :: (IntBytes a, IntBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodI64 n m k x y= case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r +# (*#) (ix (i +# n *# l) x)
                                                                          (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeInt64Array# marr (i +# n *# j) (loop' i j 0# 0#) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodI64 #-}

prodW :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ix (i +# n *# l) x)
                                                                                         (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWordArray# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodW #-}

prodW8 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW8 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ix (i +# n *# l) x)
                                                                                         (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord8Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodW8 #-}


prodW16 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW16 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ix (i +# n *# l) x)
                                                                                         (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord16Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodW16 #-}

prodW32 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW32 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ix (i +# n *# l) x)
                                                                                         (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord32Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodW32 #-}

prodW64 :: (WordBytes a, WordBytes b, PrimBytes c) => Int# -> Int# -> Int# -> a -> b -> c
prodW64 n m k x y = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) ->
           let loop' i j l r | isTrue# (l ==# m) = r
                             | otherwise = loop' i j (l +# 1#) (r `plusWord#` timesWord# (ix (i +# n *# l) x)
                                                                                         (ix (l +# m *# j) y))
           in case loop2# n k
               (\i j s' -> writeWord64Array# marr (i +# n *# j) (loop' i j 0# 0##) s'
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> fromBytes (# 0#, n *# k,  r #)
    where
      bs = n *# k *# elementByteSize x
{-# INLINE prodW64 #-}


-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s) -> State# s -> State# s
loop2# n m f = loop' 0# 0#
  where
    loop' i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop' 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop' (i +# 1#) j s1
{-# INLINE loop2# #-}


-- contract' :: forall (t :: Type) (m :: Nat) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
--            . ( ToList asbs ~ SimplifyList ('Concat (ToList as) (ToList bs))
--              , ToList as   ~ SimplifyList ('Prefix (ToList bs) (ToList asbs))
--              , ToList bs   ~ SimplifyList ('Suffix (ToList as) (ToList asbs))
--              , Dimensions asbs
--              , Dimensions (as +: m)
--              , Dimensions (m :+ bs)
--              , KnownNat m
--              , ElementDataType t
--              )
--           => DataFrame t (as +: m) -> DataFrame t (m :+ bs) -> DataFrame t asbs
-- contract' x y = case dim @asbs of
--   D -> case ( unsafeCoerce Refl :: as :~: '[]
--             , unsafeCoerce Refl :: bs :~: '[]
--             ) of
--     (Refl, Refl) -> case edtRefl (Proxy @t) of
--         EDTFloat -> contract x y
--   _ :* (sbs :: Dim (sbs :: [Nat])) -> case edtRefl (Proxy @t) of
--       EDTFloat -> contract x y
    --    case ( unsafeCoerce Refl :: EvalConsNat (SimplifyList (ToListNat sbs)) :~: sbs
    --         , unsafeCoerce Refl :: SimplifyList (ToListNat bs) :~: ToListNat bs
    --         , unsafeCoerce Refl :: ToList (as +: m) :~: SimplifyList (ToList (as +: m))
    --         ) of
    -- (Refl, Refl, Refl) -> case edtRefl (Proxy @t) of
    --     EDTFloat -> contract x y
