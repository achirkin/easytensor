{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UnboxedTuples           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpace
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (!), (!.), element
  , ewfoldMap, iwfoldMap
  , ewzip, iwzip
  , indexWise_, elementWise_
  ) where

import           GHC.Base

import           Numeric.DataFrame.Family
import           Numeric.DataFrame.Internal.Array.Class
import           Numeric.DataFrame.Internal.Array.PrimOps
import           Numeric.Dimensions
import           Numeric.PrimBytes

-- | Operations on DataFrames
--
-- @as@ is an indexing dimensionality
--
-- @bs@ is an element dimensionality
--
-- @t@ is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , Dimensions as
      , Dimensions bs
      , Dimensions asbs
      , PrimArray t (DataFrame t asbs)
      ) => SubSpace (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Unsafely get a sub-dataframe by its primitive element offset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("bs" element) size (aka `totalDim` of sub dataframe)
    --
    --   Normal indexing can be expressed in terms of `indexOffset#`:
    --
    --   > i !. x = case (# dimVal (dim @bs), fromEnum i #) of (# I# n, I# j #) -> indexOffset# (n *# j) n x
    indexOffset# :: Int# -- ^ Prim element offset
                 -> Int# -- ^ Number of prim elements in the prefix subspace
                 -> DataFrame t asbs -> DataFrame t bs
    -- | Set a new value to an element
    update :: Idxs as -> DataFrame t bs -> DataFrame t asbs -> DataFrame t asbs
    -- | Map a function over each element of DataFrame
    ewmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Map a function over each element with its index of DataFrame
    iwmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as bs' asbs'
           => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Generate a DataFrame by repeating an element
    ewgen :: DataFrame t bs -> DataFrame t asbs
    -- | Generate a DataFrame by iterating a function (index -> element)
    iwgen :: (Idxs as -> DataFrame t bs) -> DataFrame t asbs
    -- | Left-associative fold of a DataFrame.
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldl :: (b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    -- | Left-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldl :: (Idxs as -> b -> DataFrame t bs -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldr :: (DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldr :: (Idxs as -> DataFrame t bs -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Apply an applicative functor on each element (Lens-like traversal)
    elementWise :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
                 . ( Applicative f
                   , SubSpace s as bs' asbs'
                   )
                => (DataFrame s bs' -> f (DataFrame t bs))
                -> DataFrame s asbs' -> f (DataFrame t asbs)
    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal)
    indexWise :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
               . ( Applicative f
                 , SubSpace s as bs' asbs'
                 )
              => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
              -> DataFrame s asbs' -> f (DataFrame t asbs)

-- | Get an element by its index in the dataframe
(!.) :: forall t as bs asbs
      . SubSpace t as bs asbs
     => Idxs as -> DataFrame t asbs -> DataFrame t bs
(!.) i = case (# totalDim (dims @_ @bs), fromEnum i #) of
   (# W# n, I# j #) -> indexOffset# (word2Int# n *# j) (word2Int# n)
{-# INLINE [1] (!.) #-}
infixr 4 !.

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
indexWise_ :: forall t as bs asbs f b
            . (SubSpace t as bs asbs, Applicative f)
           => (Idxs as -> DataFrame t bs -> f b)
           -> DataFrame t asbs -> f ()
indexWise_ f = iwfoldr (\i -> (*>) . f i) (pure ())

-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise_ :: forall t as bs asbs f b
              . (SubSpace t as bs asbs, Applicative f)
             => (DataFrame t bs -> f b)
             -> DataFrame t asbs -> f ()
elementWise_ f = ewfoldr ((*>) . f) (pure ())


-- | Apply a functor over a single element (simple lens)
element :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) f
         . (SubSpace t as bs asbs, Applicative f)
        => Idxs as
        -> (DataFrame t bs -> f (DataFrame t bs))
        -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (update i) df <$> f (i !. df)
{-# INLINE element #-}

-- | Index an element (reverse of !.)
(!) :: SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
    => DataFrame t asbs -> Idxs as -> DataFrame t bs
(!) = flip (!.)
infixl 4 !
{-# INLINE (!) #-}


ewfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . (Monoid m, SubSpace t as bs asbs)
          => (DataFrame t bs -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldl (\m b -> m `seq` (mappend m $! f b)) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . ( Monoid m, SubSpace t as bs asbs)
          => (Idxs as -> DataFrame t bs -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldl (\i m b -> m `seq` (mappend m $! f i b)) mempty
{-# INLINE iwfoldMap #-}



-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (bs' :: [Nat]) (asbs' :: [Nat])
                r (bs'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as bs' asbs'
         , SubSpace r as bs'' asbs''
         )
      => (Idxs as -> DataFrame t bs -> DataFrame s bs' -> DataFrame r bs'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (i !. dfs)
{-# INLINE iwzip #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (bs' :: [Nat]) (asbs' :: [Nat])
                r (bs'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as bs' asbs'
         , SubSpace r as bs'' asbs''
         )
      => (DataFrame t bs -> DataFrame s bs' -> DataFrame r bs'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
ewzip = iwzip . const
{-# INLINE ewzip #-}


instance ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimArray t (DataFrame t bs)
         , PrimArray t (DataFrame t asbs)
         , PrimBytes   (DataFrame t bs)
         , PrimBytes   (DataFrame t asbs)
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where

    -- indexOffset# i l d = case numElems0 d of
    --   -- if numElems0 returns 0, then this is a fromScalar-like constructor
    --   0# -> broadcast (ix# 0# d)
    --   _  -> fromElems (offsetElems d +# i) l (getBytes d)

    --
    -- update ei x df
    --   | I# i <- fromEnum ei
    --   , I# len <- fromIntegral $ totalDim' @asbs
    --   = case runRW#
    --       ( \s0 -> case newByteArray# (len *# byteSize @t undefined) s0 of
    --         (# s1, mba #) -> unsafeFreezeByteArray# mba
    --           ( writeArray mba i x
    --             ( writeBytes mba 0# df s1 )
    --           )
    --       ) of (# _, r #) -> fromElems 0# len r
    --
    -- ewmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
    --         . SubSpace s as bs' asbs'
    --        => (DataFrame s bs' -> DataFrame t bs)
    --        -> DataFrame s asbs' -> DataFrame t asbs
    -- ewmap f df
    --   | elS <- byteSize @t undefined
    --   , W# lenBSW <- totalDim' @bs
    --   , W# lenASW <- totalDim' @as
    --   , W# lenAS'W <- totalDim' @as'
    --   , lenBS <- word2Int# lenBSW
    --   , lenAS <- word2Int# lenASW
    --   , lenAS' <- word2Int# lenAS'W
    --   , lenASBS <- lenAS *# lenBS
    --   , lenAS'BS <- lenAS' *# lenBS
    --   = case runRW#
    --       ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
    --         (# s1, mba #) -> unsafeFreezeByteArray# mba
    --           ( loopWithI# 0# lenAS' lenAS'BS
    --             (\i off -> writeArray mba i (f (indexOffset# off lenAS' df)))
    --             s1
    --           )
    --       ) of (# _, r #) -> fromElems 0# lenASBS r
    -- {-# INLINE ewmap #-}
    --
    --
    -- iwmap  :: forall s (bs' :: [Nat]) (asbs' :: [Nat])
    --         . SubSpace s as bs' asbs'
    --        => (Idxs as -> DataFrame s bs' -> DataFrame t bs)
    --        -> DataFrame s asbs' -> DataFrame t asbs
    -- iwmap f df
    --   | elS <- byteSize @t undefined
    --   , dbs <- dims @_ @bs
    --   , W# lenBSW <- totalDim dbs
    --   , W# lenASW <- totalDim' @as
    --   , W# lenAS'W <- totalDim' @as'
    --   , lenBS <- word2Int# lenBSW
    --   , lenAS <- word2Int# lenASW
    --   , lenAS' <- word2Int# lenAS'W
    --   , lenASBS <- lenAS *# lenBS
    --   = case runRW#
    --       ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
    --         (# s1, mba #) -> unsafeFreezeByteArray# mba
    --           ( overDim_# dbs
    --             ( \i pos ->
    --                 writeArray mba pos (f i (indexOffset# (pos *# lenAS') lenAS' df))
    --             ) 0# 1# s1
    --           )
    --       ) of (# _, r #) -> fromElems 0# lenASBS r
    --
    -- ewgen x = case numElems0 x of
    --   0# -> broadcast (ix# 0# x)
    --   1# -> broadcast (ix# 0# x)
    --   lenAS
    --     | elS <- byteSize @t undefined
    --     , W# lenBSW <- totalDim' @bs
    --     , lenBS <- word2Int# lenBSW
    --     , lenASBS <- lenAS *# lenBS
    --     , bsize <- lenASBS *# elS
    --     -> case runRW#
    --         ( \s0 -> case newByteArray# bsize s0 of
    --           (# s1, mba #) -> unsafeFreezeByteArray# mba
    --             ( loop# 0# (lenAS *# elS) bsize
    --               (\off -> writeBytes mba off x)
    --               s1
    --             )
    --         ) of (# _, r #) -> fromElems 0# lenASBS r
    -- {-# INLINE [1] ewgen #-}
    --
    -- iwgen f
    --   | elS <- byteSize @t undefined
    --   , dbs <- dims @_ @bs
    --   , W# lenBSW <- totalDim dbs
    --   , W# lenASW <- totalDim' @as
    --   , lenBS <- word2Int# lenBSW
    --   , lenAS <- word2Int# lenASW
    --   , lenASBS <- lenAS *# lenBS
    --   = case runRW#
    --       ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
    --         (# s1, mba #) -> unsafeFreezeByteArray# mba
    --           ( overDim_# dbs
    --             ( \i pos -> writeArray mba pos (f i)
    --             ) 0# 1# s1
    --           )
    --       ) of (# _, r #) -> fromElems 0# lenASBS r
    --
    -- ewfoldl f x0 df
    --   | ba <- getBytes df
    --   = foldDimOff (dims @_ @bs) (\(I# o) acc -> f acc (fromBytes o ba))
    --       (I# (byteOffset df))
    --       (I# (byteSize @t undefined) * fromIntegral (totalDim' @as)) x0
    --
    -- iwfoldl f x0 df
    --   | ba <- getBytes df
    --   = foldDim (dims @_ @bs) (\i (I# o) acc -> f i acc (fromBytes o ba))
    --       (I# (byteOffset df))
    --       (I# (byteSize @t undefined) * fromIntegral (totalDim' @as)) x0
    --
    -- ewfoldr f x0 df
    --   | step <- I# (byteSize @t undefined) * fromIntegral (totalDim' @as)
    --   , ba <- getBytes df
    --   = foldDimOff (dims @_ @bs) (\(I# o) -> f (fromBytes o ba))
    --       (I# (byteOffset df +# byteSize df) - step)
    --       (negate step) x0
    --
    -- iwfoldr f x0 df
    --   | step <- I# (byteSize @t undefined) * fromIntegral (totalDim' @as)
    --   , ba <- getBytes df
    --   = foldDimReverse (dims @_ @bs) (\i (I# o) -> f i (fromBytes o ba))
    --       (I# (byteOffset df +# byteSize df) - step)
    --       step x0
    --
    --
    -- -- implement elementWise in terms of indexWise
    -- elementWise = indexWise . const
    -- {-# INLINE elementWise #-}
    --
    -- indexWise :: forall (s :: Type) (bs' :: [Nat]) (asbs' :: [Nat]) (f :: Type -> Type)
    --            . ( Applicative f
    --              , SubSpace s as bs' asbs'
    --              )
    --           => (Idxs as -> DataFrame s bs' -> f (DataFrame t bs))
    --           -> DataFrame s asbs' -> f (DataFrame t asbs)
    -- indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
    --   where
    --     -- run a state-based continuation within RW
    --     runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    --                  -> DataFrame t asbs
    --     runWithState g = case runRW#
    --                        ( \s0 -> case g s0 of
    --                             (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
    --                        ) of (# _, arr #) -> fromElems 0# rezLength# arr
    --
    --     -- Prepare empty byte array for the result DataFrame and keep a current position counter
    --     -- Input: state
    --     -- Output: state +
    --     --     ( current mutable byte array + current write position )
    --     initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
    --     initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
    --                         (# s1, marr #) -> (# s1, (# marr, 0# #) #)
    --
    --     -- Given the result chunk, write it into a mutable array
    --     updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    --                 -> DataFrame t as
    --                 -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    --     updateChunk g dfChunk = case (# byteOffset dfChunk, getBytes dfChunk #) of
    --         (# off#, arr#  #) -> \s -> case g s of
    --                                     (# s1, (# marr#, pos# #) #) -> case
    --                                         copyByteArray# arr# (off# *# rezElBSize#)
    --                                                        marr# (pos# *# rezElBSize#)
    --                                                        (rezStepN# *# rezElBSize#) s1 of
    --                                       s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)
    --
    --     -- Apply applicative functor on each chunk and update a state.
    --     applyF :: Idxs bs
    --            -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    --            -> DataFrame s as'
    --            -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    --     applyF idx s dfChunk = idx `seq` dfChunk `seq` updateChunk <$> s <*> f idx dfChunk
    --
    --     -- Element byte size of the result DataFrame (byte size of s)
    --     rezElBSize# = byteSize @t undefined
    --     -- Number of primitive elements in the result DataFrame chunk
    --     !(I# rezStepN#) = fromIntegral $ totalDim' @as
    --     -- Number of primitive elements in the result DataFrame
    --     !(I# rezLength#) = fromIntegral $ totalDim' @asbs

--
-- unSc :: DataFrame (t :: Type) ('[] :: [Nat]) -> t
-- unSc = unsafeCoerce#
--
-- {-# RULES
-- "ewgen/broadcast" ewgen = broadcast . unSc
--
--   #-}
