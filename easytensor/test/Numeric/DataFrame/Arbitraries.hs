{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Provide instance of Arbitrary for all DataFrame types.
--   Also, this module is an example of fancy type inference and DataFrame
--   traversals with monadic actions.
module Numeric.DataFrame.Arbitraries where

import           Test.QuickCheck

import           Numeric.DataFrame
import           Numeric.Dimensions
import           Numeric.PrimBytes

instance (Arbitrary t, PrimBytes t, Dimensions ds)
      => Arbitrary (DataFrame t (ds :: [Nat])) where
    arbitrary
        | -- First, we need to find out exact array implementation to use
          -- inside this DataFrame.
          -- We need to do that whenever exact value of ds is not known
          E <- inferASing' @t @ds
          -- Then, we need to get basic byte manipulation type classes, such as
          -- PrimBytes and PrimArray.
        , E <- inferPrim' @t @ds
          -- After that, GHC can infer all necessary fancy things like SubSpace
          -- to do complex operations on sub-dimensions of a DataFrame.
          --
          -- Note, we could put SubSpace into constraints of this instance as well.
          -- That would render the above lines unnecessary, but would make
          -- inference more difficult later.
        = arbitrary >>= elementWise @_ @_ @ds f . ewgen . scalar
      where
        f :: Arbitrary a => Scalar a -> Gen (Scalar a)
        f _ = scalar <$> arbitrary
    shrink
        | E <- inferASing' @t @ds
        , E <- inferPrim' @t @ds
        = elementWise @_ @_ @ds f
      where
        -- Unfortunately, Scalar is not a proper second-rank data type
        -- (it is just type alias for DataFrame t []).
        -- So it cannot be functor or traversable.
        f :: Arbitrary a => Scalar a -> [Scalar a]
        f = fmap scalar . shrink . unScalar

instance (All Arbitrary ts, All PrimBytes ts, RepresentableList ts, Dimensions ds)
      => Arbitrary (DataFrame ts (ds :: [Nat])) where
    -- We create arbitrary MultiFrame by combining several SingleFrames.
    -- SingleFrames are "variables" or "columns" of a MultiFrame that are
    -- independent byte arrays bounded by a common dimensions type signature.
    arbitrary = -- Use RepresentableList to find out how many columns are there.
                case tList @_ @ts of
        -- Zero columns, empty MultiFrame
        U -> return Z
        -- Cons-like construction.
        -- Note, pattern matching TypeList brings RepresentableList evidence
        -- for Tail ts.
        _ :* (TypeList :: TypeList ts') -> do
          at   <- arbitrary
          ats' <- arbitrary @(DataFrame ts' ds)
          return (at :*: ats')
    shrink Z = []
    -- MultiFrame is a newtype wrapper on a TypedList.
    -- Thus, we can always recover RepresentableList ts by using function @types@
    shrink (at :*: ats@(MultiFrame ats'))
      | TypeList <- types ats'
      = (:*:) <$> shrink at <*> shrink ats


maxDims :: Word
maxDims = 5

maxDimSize :: Word
maxDimSize = 7

instance Arbitrary SomeDims where
    arbitrary = do
      dimN <- choose (0, maxDims) :: Gen Word
      wdims <- mapM (\_ -> choose (2, maxDimSize) :: Gen Word) [1..dimN]
      return $ someDimsVal wdims

instance (Arbitrary t, PrimBytes t)
      => Arbitrary (SomeDataFrame t) where
    arbitrary = do
      -- Generate random dimension list
      --  and pattern-match against it with Dims pattern.
      --  This gives Dimensions ds evidence immediately.
      SomeDims (Dims :: Dims ds) <- arbitrary
      -- We also need to figure out an array implementation...
      case inferASing' @t @ds of
        -- ... and generating a random DataFrame becomes a one-liner
        E -> SomeDataFrame <$> arbitrary @(DataFrame t ds)

-- All same as above, just change constraints a bit
instance (All Arbitrary ts, All PrimBytes ts, RepresentableList ts)
      => Arbitrary (SomeDataFrame ts) where
    arbitrary = do
      SomeDims (Dims :: Dims ds) <- arbitrary
      case inferASing' @ts @ds of
        E -> SomeDataFrame <$> arbitrary @(DataFrame ts ds)
