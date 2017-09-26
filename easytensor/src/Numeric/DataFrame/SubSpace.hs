{-# LANGUAGE CPP                     #-}
{-# LANGUAGE BangPatterns            #-}
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
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI           #-}
{-# LANGUAGE UnliftedFFITypes        #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpace
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------

module Numeric.DataFrame.SubSpace
  ( SubSpace (..), (!), element
  , ewfoldMap, iwfoldMap
  , ewzip, iwzip
  ) where

import           GHC.Base                  (runRW#)
import           GHC.Prim
import           GHC.Types                 (Int (..), Type)

#ifdef ghcjs_HOST_OS
import           GHCJS.Types (JSVal)
import           Unsafe.Coerce (unsafeCoerce)
#endif

import qualified Numeric.Array.ElementWise as EW
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits
import           Numeric.Scalar

-- | Operations on DataFrames
--
-- @as@ is an element dimensionality
--
-- @bs@ is an indexing dimensionality
--
-- @t@ is an underlying data type (i.e. Float, Int, Double)
--
class ( ConcatList as bs asbs
      , Dimensions as
      , Dimensions bs
      , Dimensions asbs
      ) => SubSpace (t :: Type) (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                    | asbs as -> bs, asbs bs -> as, as bs -> asbs where
    -- | Unsafely get a sub-dataframe by its primitive element subset.
    --   The offset is not checked to be aligned to the space structure or for bounds.
    --   Arguments are zero-based primitive element offset and subset ("as" element) size (aka `totalDim` of sub dataframe)
    --
    --   Normal indexing can be expressed in terms of `indexOffset#`:
    --
    --   > i !. x = case (# dimVal (dim @as), fromEnum i #) of (# I# n, I# j #) -> indexOffset# (n *# j) n x
    indexOffset# :: Int# -> Int# -> DataFrame t asbs -> DataFrame t as
    -- | Get an element by its index in the dataframe
    (!.) :: Idx bs -> DataFrame t asbs -> DataFrame t as
    (!.) i = case (# dimVal (dim @as), fromEnum i #) of (# I# n, I# j #) -> indexOffset# (n *# j) n
    {-# INLINE (!.) #-}
    -- | Set a new value to an element
    update :: Idx bs -> DataFrame t as -> DataFrame t asbs -> DataFrame t asbs
    -- | Map a function over each element of DataFrame
    ewmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Map a function over each element with its index of DataFrame
    iwmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (Idx bs -> DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    -- | Generate a DataFrame by repeating an element
    ewgen :: DataFrame t as -> DataFrame t asbs
    -- | Generate a DataFrame by iterating a function (index -> element)
    iwgen :: (Idx bs -> DataFrame t as) -> DataFrame t asbs
    -- | Left-associative fold of a DataFrame.
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldl :: (b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Left-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldl :: (Idx bs -> b -> DataFrame t as -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    ewfoldr :: (DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Right-associative fold of a DataFrame with an index
    --   The fold is strict, so accumulater is evaluated to WHNF;
    --   but you'd better make sure that the function is strict enough to not
    --   produce memory leaks deeply inside the result data type.
    iwfoldr :: (Idx bs -> DataFrame t as -> b -> b) -> b -> DataFrame t asbs -> b
    -- | Apply an applicative functor on each element (Lens-like traversal)
    elementWise :: forall s (as' :: [Nat]) (asbs' :: [Nat]) f
                 . ( Applicative f
                   , SubSpace s as' bs asbs'
                   )
                => (DataFrame s as' -> f (DataFrame t as))
                -> DataFrame s asbs' -> f (DataFrame t asbs)
    -- | Apply an applicative functor on each element with its index
    --     (Lens-like indexed traversal)
    indexWise :: forall s (as' :: [Nat]) (asbs' :: [Nat]) f
               . ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame s as' -> f (DataFrame t as))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
infixr 4 !.

-- | Apply a functor over a single element (simple lens)
element :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) f
         . (SubSpace t as bs asbs, Applicative f)
        => Idx bs
        -> (DataFrame t as -> f (DataFrame t as))
        -> DataFrame t asbs -> f (DataFrame t asbs)
element i f df = flip (update i) df <$> f (i !. df)
{-# INLINE element #-}

-- | Index an element (reverse of !.)
(!) :: SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
    => DataFrame t asbs -> Idx bs -> DataFrame t as
(!) = flip (!.)
infixl 4 !
{-# INLINE (!) #-}


ewfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . (Monoid m, SubSpace t as bs asbs)
          => (DataFrame t as -> m) -> DataFrame t asbs -> m
ewfoldMap f = ewfoldl (\m b -> m `seq` (mappend m $! f b)) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . ( Monoid m, SubSpace t as bs asbs)
          => (Idx bs -> DataFrame t as -> m) -> DataFrame t asbs -> m
iwfoldMap f = iwfoldl (\i m b -> m `seq` (mappend m $! f i b)) mempty
{-# INLINE iwfoldMap #-}



-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (as' :: [Nat]) (asbs' :: [Nat])
                r (as'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as' bs asbs'
         , SubSpace r as'' bs asbs''
         )
      => (Idx bs -> DataFrame t as -> DataFrame s as' -> DataFrame r as'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (i !. dfs)
{-# INLINE iwzip #-}

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat])
                s (as' :: [Nat]) (asbs' :: [Nat])
                r (as'' :: [Nat]) (asbs'' :: [Nat])
       . ( SubSpace t as bs asbs
         , SubSpace s as' bs asbs'
         , SubSpace r as'' bs asbs''
         )
      => (DataFrame t as -> DataFrame s as' -> DataFrame r as'')
      -> DataFrame t asbs
      -> DataFrame s asbs'
      -> DataFrame r asbs''
ewzip = iwzip . const
{-# INLINE ewzip #-}


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$3.subarray($1,$1 + $2)" js_subarray        :: Int# -> Int# -> JSVal -> JSVal
#endif

instance {-# OVERLAPPABLE #-}
         ( ConcatList as bs asbs
         , Dimensions as
         , Dimensions bs
         , Dimensions asbs
         , PrimBytes (DataFrame t as)
         , PrimBytes (DataFrame t asbs)
         , as ~ (a'' ': as'')
         , asbs ~ (a'' ': asbs'')
         ) => SubSpace t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) where
#ifdef ghcjs_HOST_OS
    indexOffset# i l = unsafeCoerce . js_subarray i l . unsafeCoerce
#else
    indexOffset# i l d = case toBytes d of
        (# off, _, arr #) -> fromBytes (# off +# i, l, arr #)
#endif
    {-# INLINE indexOffset# #-}

    ewmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    ewmap f df
      | elS <- elementByteSize (undefined :: DataFrame t asbs)
      , I# lenBS <- totalDim (Proxy @bs)
      , I# lenAS <- totalDim (Proxy @as)
      , I# lenAS' <- totalDim (Proxy @as')
      , lenASB <- lenAS *# elS
      = case runRW#
          ( \s0 -> case newByteArray# (lenAS *# lenBS *# elS) s0 of
              (# s1, marr #) -> case overDimOff_#
                  (dim @bs)
                  ( \pos s -> case toBytes $ f (indexOffset# (pos *# lenAS') lenAS' df) of
                      (# offX, _, arrX #) -> copyByteArray# arrX (offX *# elS) marr (pos *# lenASB) lenASB s
                  ) 0# 1# s1 of
                s2 -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenAS *# lenBS, r #)
    {-# INLINE ewmap #-}

    iwmap  :: forall s (as' :: [Nat]) (asbs' :: [Nat])
            . SubSpace s as' bs asbs'
           => (Idx bs -> DataFrame s as' -> DataFrame t as)
           -> DataFrame s asbs' -> DataFrame t asbs
    iwmap f df
      | elS <- elementByteSize (undefined :: DataFrame t asbs)
      , I# lenBS <- totalDim (Proxy @bs)
      , I# lenAS <- totalDim (Proxy @as)
      , I# lenAS' <- totalDim (Proxy @as')
      , lenASB <- lenAS *# elS
      = case runRW#
          ( \s0 -> case newByteArray# (lenAS *# lenBS *# elS) s0 of
              (# s1, marr #) -> case overDim_#
                  (dim @bs)
                  ( \i pos s -> case toBytes $ f i (indexOffset# (pos *# lenAS') lenAS' df) of
                      (# offX, _, arrX #) -> copyByteArray# arrX (offX *# elS) marr (pos *# lenASB) lenASB s
                  ) 0# 1# s1 of
                s2 -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenAS *# lenBS, r #)

    ewgen x
      | (# offX, lenX, arrX #) <- toBytes x
      , I# lenASBS <- totalDim (Proxy @asbs)
      , elS <- elementByteSize x
      , offXB <- offX *# elS
      , lenXB <- lenX *# elS
      = case runRW#
          ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
              (# s1, marr #) -> case overDimOff_# (dim @bs)
                  ( \posB -> copyByteArray# arrX offXB marr posB lenXB )
                  0# lenXB s1 of
                s2 -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenASBS, r #)

    iwgen f
      | I# lenASBS <- totalDim (Proxy @asbs)
      , elS <- elementByteSize (undefined :: DataFrame t asbs)
      , I# lenAS <- totalDim (Proxy @as)
      , lenASB <- lenAS *# elS
      = case runRW#
          ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
              (# s1, marr #) -> case overDim_# (dim @bs)
                  ( \i pos s -> case toBytes (f i) of
                      (# offX, _, arrX #) -> copyByteArray# arrX (offX *# elS) marr pos lenASB s
                  ) 0# lenASB s1 of
                s2 -> unsafeFreezeByteArray# marr s2
          ) of (# _, r #) -> fromBytes (# 0#, lenASBS, r #)

    ewfoldl f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, _, arr #), I# step #) -> foldDimOff (dim @bs)
                    (\pos acc -> f acc $! fromBytes (# pos, step, arr #))
                    off step x0

    iwfoldl f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, _, arr #), I# step #) -> foldDim (dim @bs)
                    (\i pos acc -> f i acc $! fromBytes (# pos, step, arr #))
                    off step x0

    ewfoldr f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, len, arr #), I# step #) -> foldDimOff (dim @bs)
                    (\pos -> f (fromBytes (# pos, step, arr #)))
                    (off +# len -# step) (negateInt# step) x0

    iwfoldr f x0 df = case (# toBytes df, totalDim ( Proxy @as) #) of
        (# (# off, _, arr #), I# step #) -> foldDimReverse (dim @bs)
                    (\i pos -> f i (fromBytes (# pos, step, arr #)) )
                    off step x0

    -- implement elementWise in terms of indexWise
    elementWise = indexWise . const
    {-# INLINE elementWise #-}

    indexWise :: forall (s :: Type) (f :: Type -> Type) (as' :: [Nat]) (asbs' :: [Nat])
               . ( Applicative f
                 , SubSpace s as' bs asbs'
                 )
              => (Idx bs -> DataFrame s as' -> f (DataFrame t as))
              -> DataFrame s asbs' -> f (DataFrame t asbs)
    indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
      where
        -- run a state-based continuation within RW
        runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                     -> DataFrame t asbs
        runWithState g = case runRW#
                           ( \s0 -> case g s0 of
                                (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
                           ) of (# _, arr #) -> fromBytes (# 0#, rezLength#, arr #)

        -- Prepare empty byte array for the result DataFrame and keep a current position counter
        -- Input: state
        -- Output: state +
        --     ( current mutable byte array + current write position )
        initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
        initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
                            (# s1, marr #) -> (# s1, (# marr, 0# #) #)

        -- Given the result chunk, write it into a mutable array
        updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                    -> DataFrame t as
                    -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        updateChunk g dfChunk = case toBytes dfChunk of
            (# off#, _, arr#  #) -> \s -> case g s of
                                        (# s1, (# marr#, pos# #) #) -> case
                                            copyByteArray# arr# (off# *# rezElBSize#)
                                                           marr# (pos# *# rezElBSize#)
                                                           (rezStepN# *# rezElBSize#) s1 of
                                          s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)

        -- Apply applicative functor on each chunk and update a state.
        applyF :: Idx bs
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
               -> DataFrame s as'
               -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
        applyF idx s dfChunk = idx `seq` dfChunk `seq` updateChunk <$> s <*> f idx dfChunk

        -- Element byte size of the result DataFrame (byte size of s)
        rezElBSize# = elementByteSize (undefined :: DataFrame t asbs)
        -- Number of primitive elements in the result DataFrame chunk
        !(I# rezStepN#) = totalDim (Proxy @as)
        -- Number of primitive elements in the result DataFrame
        !(I# rezLength#) = totalDim (Proxy @asbs)

    update ei x df
      | I# i <- fromEnum ei
      , (# off, len, arr #) <- toBytes df
      , (# offX, lenX, arrX #) <- toBytes x
      , elS <- elementByteSize df
      = case runRW#
          ( \s0 -> case newByteArray# ( len *# elS ) s0 of
            (# s1, marr #) -> case copyByteArray# arr (off *# elS) marr 0# (len *# elS) s1 of
              s2 -> case copyByteArray# arrX (offX *# elS) marr (lenX *# i *# elS) (lenX *# elS) s2 of
                s3 -> unsafeFreezeByteArray# marr s3
          ) of (# _, r #) -> fromBytes (# 0#, len, r #)




-- | Specialized instance of SubSpace for operating on scalars.
instance {-# OVERLAPPING #-}
         ( Dimensions bs
         , EW.ElementWise (Idx bs) t (DataFrame t bs)
         , PrimBytes (DataFrame t bs)
         ) => SubSpace t ('[] :: [Nat]) (bs :: [Nat]) (bs :: [Nat]) where
    indexOffset# i _ x = scalar (EW.indexOffset# x i)
    {-# INLINE indexOffset# #-}
    i !. x =  scalar $ x EW.! i
    {-# INLINE (!.) #-}
    ewmap = iwmap . const
    {-# INLINE ewmap #-}
    iwmap f x = EW.ewgen (\i -> unScalar $ f i (i !. x))
    {-# INLINE iwmap #-}
    ewgen = EW.broadcast . unScalar
    {-# INLINE ewgen #-}
    iwgen f = EW.ewgen (unScalar . f)
    {-# INLINE iwgen #-}
    ewfoldl f = EW.ewfoldl (\_ a -> f a . scalar)
    {-# INLINE ewfoldl #-}
    iwfoldl f = EW.ewfoldl (\i a -> f i a . scalar)
    {-# INLINE iwfoldl #-}
    ewfoldr f = EW.ewfoldr (\_ x  -> f (scalar x))
    {-# INLINE ewfoldr #-}
    iwfoldr f = EW.ewfoldr (\i x -> f i (scalar x))
    {-# INLINE iwfoldr #-}
    elementWise = indexWise . const
    {-# INLINE elementWise #-}
    indexWise f x = EW.ewgenA (\i -> unScalar <$> f i (i !. x))
    {-# INLINE indexWise #-}
    update i x = EW.update i (unScalar x)
    {-# INLINE update #-}
