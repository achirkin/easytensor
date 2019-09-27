{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
module Numeric.Subroutine.Sort
  ( SortBy (..), sortBy, sort
  , SortableDataFrame
  ) where


import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Kind
import Data.Type.Lits
import Numeric.DataFrame.Internal.PrimArray
import Numeric.DataFrame.ST
import Numeric.DataFrame.Type
import Numeric.Dimensions
import Unsafe.Coerce

-- | Sort a @DataFrame@ along the first dimension.
--
--   Note: the elements (which are of type @DataFrame t ns@) are compared
--         lexicographically.
sort :: forall (t :: Type) n ns
      . ( SortableDataFrame t (n ': ns), Ord t, SortBy n)
     => DataFrame t (n ': ns)
     -> DataFrame t (n ': ns)
sort df = case dimKind @(KindOf n) of
    DimKNat  -> case uniqueOrCumulDims df of
      Left _ -> df -- all equal, no need for sorting.
      Right steps
        | SomeDims (Dims :: Dims ms) <- fromSteps steps
        , Dict <- (unsafeCoerce (Dict @(ns ~ ns)) :: Dict (ns ~ ms))
          -> sortBy compare df
        | otherwise
          -> error "sort/DimNat/uniqueOrCumulDims -- impossible pattern"
    DimKXNat
        | XFrame (df' :: DataFrame t ms) <- df
        , D :* Dims <- dims @ms
          -> XFrame (sortBy compare df')
        | otherwise
          -> error "sort/DimXNat -- impossible pattern"

-- | Sort a @DataFrame@ along the first dimension using given comparison function.
sortBy :: forall (t :: Type) n ns
        . ( SortableDataFrame t (n ': ns)
          , SortBy n)
       => (DataFrame t ns -> DataFrame t ns -> Ordering)
       -> DataFrame t (n ': ns)
       -> DataFrame t (n ': ns)
sortBy cmp df = case dimKind @(KindOf n) of
    DimKNat
      | Left _ <- uniqueOrCumulDims df -> df -- all equal, no need for sorting.
      | otherwise -> runST $ do
        mdf <- uncheckedThawDataFrame df
        sortByInplace
          (\x y -> cmp <$> unsafeFreezeDataFrame x <*> unsafeFreezeDataFrame y)
          mdf
        unsafeFreezeDataFrame mdf
    DimKXNat
      | XFrame dfN <- df
      , D :* Dims <- dims `inSpaceOf` dfN
        -> XFrame (sortBy (\a b -> cmp (XFrame a) (XFrame b)) dfN)
      | otherwise
        -> error "sortBy/DimXNat -- impossible pattern"

-- | The required context for sorting a DataFrame is slightly different
--   for @Nat@ and @XNat@ indexed arrays.
--   This type family abstracts away the difference.
type family SortableDataFrame (t :: Type) (ns :: [k]) :: Constraint where
    SortableDataFrame t ((n ': ns) :: [Nat])
      = (PrimArray t (DataFrame t ns), PrimArray t (DataFrame t (n ': ns)))
    SortableDataFrame t ((n ': ns) :: [XNat])
      = PrimBytes t


class BoundedDim n => SortBy n where
    -- | Note, "Inplace" here means the input frame is modified.
    --   It does not mean the algorithm does not use extra space (it does use).
    sortByInplace :: PrimBytes t
                  => (STDataFrame s t ns -> STDataFrame s t ns -> ST s Ordering)
                      -- ^ must not modify state!
                  -> STDataFrame s t (n ': ns)
                  -> ST s ()


instance SortBy 0 where
    sortByInplace _ _ = pure ()

instance SortBy 1 where
    sortByInplace _ _ = pure ()

instance SortBy 2 where
    sortByInplace cmp xs = cmp a b >>= \case
        GT -> do
          tmp <- oneMoreDataFrame a
          swapDF tmp a b
        _  -> pure ()
      where
        a = subDataFrameView' (Idx 0 :* U) xs
        b = subDataFrameView' (Idx 1 :* U) xs

instance SortBy 3 where
    sortByInplace cmp xs = join $
        go <$> (unsafeDupableInterleaveST (oneMoreDataFrame a))
           <*> cmp a b <*> cmp b c <*> cmp a c
      where
        a = subDataFrameView' (Idx 0 :* U) xs
        b = subDataFrameView' (Idx 1 :* U) xs
        c = subDataFrameView' (Idx 2 :* U) xs
        go tmp GT LT GT -- b < c < a
          = swap3DF tmp a b c
        go tmp LT GT GT -- c < a < b
          = swap3DF tmp b a c
        go tmp GT bc ac | bc /= GT && ac /= GT
          = swapDF tmp a b
        go tmp ab GT ac | ab /= GT && ac /= GT
          = swapDF tmp b c
        go tmp ab bc GT | ab /= LT && bc /= LT
          = swapDF tmp a c
        go _ _ _ _ = pure ()

instance SortBy 4 where
    sortByInplace cmp xs = do
        tmp <- unsafeDupableInterleaveST (oneMoreDataFrame a)
        cmpSwap tmp a c
        cmpSwap tmp b d
        cmpSwap tmp a b
        cmpSwap tmp c d
        cmpSwap tmp b c
      where
        a = subDataFrameView' (Idx 0 :* U) xs
        b = subDataFrameView' (Idx 1 :* U) xs
        c = subDataFrameView' (Idx 2 :* U) xs
        d = subDataFrameView' (Idx 3 :* U) xs
        cmpSwap tmp x y = cmp x y >>= \case
          GT -> swapDF tmp x y
          _  -> pure ()

instance {-# INCOHERENT #-}
         KnownDim n => SortBy (n :: Nat) where
    sortByInplace cmp (xs :: STDataFrame s t (n ': ns)) = do
        tmp <- oneMoreDataFrame xs
        copyMutableDataFrame' U xs tmp
        mergeSort D tmp xs
      where
        mergeSort :: Dim (d :: Nat)
                  -> STDataFrame s t (d ': ns)
                  -> STDataFrame s t (d ': ns)
                  -> ST s ()
        mergeSort D0 _ _ = pure ()
        mergeSort D1 _ _ = pure ()
        mergeSort (d@D :: Dim d) b a = do
          d2l@D <- pure $ divDim d D2
          Just d2r@D <- pure $ minusDimM d d2l
          d2li@D <- pure $ plusDim d2l D1
          d2ri@D <- pure $ plusDim d2r D1
          Just Dict <- pure $ sameDim (plusDim d D1) (plusDim d2li d2r)
          Just Dict <- pure $ sameDim (plusDim d D1) (plusDim d2ri d2l)
          let leA = subDataFrameView @t @_ @d @(d - Div d 2 + 1) @(Div d 2) @'[]
                                     (Idx 0 :* U) a
              riA = subDataFrameView @t @_ @d @(Div d 2 + 1) @(d - Div d 2) @'[]
                                     (Idx (dimVal d2l) :* U) a
              leB = subDataFrameView @t @_ @d @(d - Div d 2 + 1) @(Div d 2) @'[]
                                     (Idx 0 :* U) b
              riB = subDataFrameView @t @_ @d @(Div d 2 + 1) @(d - Div d 2) @'[]
                                     (Idx (dimVal d2l) :* U) b
          mergeSort d2l leA leB
          mergeSort d2r riA riB
          merge d2l d2r d leB riB a
        merge :: forall (a :: Nat) (b :: Nat) (ab :: Nat)
               . Dim a -> Dim b -> Dim ab
              -> STDataFrame s t (a ': ns)
              -> STDataFrame s t (b ': ns)
              -> STDataFrame s t (ab ': ns)
              -> ST s ()
        merge da@D db@D dab@D a b ab = foldM_ f (Just (0,0)) [0 .. dimVal dab - 1]
          where
            f Nothing _ = pure Nothing
            f (Just (i,j)) k
              | i >= dimVal da
              , Dx dj@(D :: Dim j) <- someDimVal j
              , D <- plusDim dj D1
              , Just bmj@D <- minusDimM db dj
              , Just bmji@D <- minusDimM (plusDim dab D1) bmj
              , Just Dict <- sameDim (plusDim dab D1) (plusDim bmji bmj)
              , Just Dict <- sameDim (plusDim db D1) (dj `plusDim` D1 `plusDim` bmj)
                = Nothing <$ copyMutableDataFrame @t @_ @ab @(ab + 1 - (b - j))
                                                            @(b - j) (Idx k :* U)
                    (subDataFrameView @t @_ @b @(j + 1) @(b - j) (Idx j :* U) b) ab
              | j >= dimVal db
              , Dx di@(D :: Dim i) <- someDimVal i
              , D <- plusDim di D1
              , Just bmi@D <- minusDimM da di
              , Just bmii@D <- minusDimM (plusDim dab D1) bmi
              , Just Dict <- sameDim (plusDim dab D1) (plusDim bmii bmi)
              , Just Dict <- sameDim (plusDim da D1) (di `plusDim` D1 `plusDim` bmi)
                = Nothing <$ copyMutableDataFrame (Idx k :* U)
                    (subDataFrameView @t @_ @a @(i + 1) @(a - i) (Idx i :* U) a) ab
              | otherwise
                = cmp (subDataFrameView' (Idx i :* U) a)
                      (subDataFrameView' (Idx j :* U) b) >>= \case
                    GT -> Just (i, j + 1)
                          <$ copyMutableDataFrame' (Idx k :* U)
                                         (subDataFrameView' (Idx j :* U) b) ab
                    _ -> Just (i + 1, j)
                          <$ copyMutableDataFrame' (Idx k :* U)
                                         (subDataFrameView' (Idx i :* U) a) ab


instance BoundedDim xn => SortBy (xn :: XNat) where
    sortByInplace cmp (XSTFrame xs)
      | D :* _ <- dims `inSpaceOf` xs
        = sortByInplace (\x y -> cmp (castDataFrame x) (castDataFrame y)) xs
      | otherwise = error "sortByInplace: impossible pattern"


-- | Swap contents of two DataFrames
swapDF :: forall (s :: Type) (t :: Type) (ns :: [Nat])
        . PrimBytes t
       => STDataFrame s t ns -- ^ Temporary buffer
       -> STDataFrame s t ns
       -> STDataFrame s t ns
       -> ST s ()
swapDF tmp a b = do
  copyMutableDataFrame' U a tmp
  copyMutableDataFrame' U b a
  copyMutableDataFrame' U tmp b

-- | Rotate left contents of three DataFrames
swap3DF :: forall (s :: Type) (t :: Type) (ns :: [Nat])
        . PrimBytes t
       => STDataFrame s t ns -- ^ Temporary buffer
       -> STDataFrame s t ns
       -> STDataFrame s t ns
       -> STDataFrame s t ns
       -> ST s ()
swap3DF tmp a b c = do
  copyMutableDataFrame' U a tmp
  copyMutableDataFrame' U b a
  copyMutableDataFrame' U c b
  copyMutableDataFrame' U tmp c
