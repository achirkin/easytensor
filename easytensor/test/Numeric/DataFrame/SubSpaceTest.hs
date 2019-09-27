{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- overwise GHC dies trying to optimize 100 equal funs in each test
{-# OPTIONS_GHC -O0       #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import Data.Typeable       (typeOf)
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Test.QuickCheck

prop_IndexFrame0 :: Property
prop_IndexFrame0 = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeQ
    (_, ds) <- someLenAsBsQ 0
    [e| do
      df <- arbitrary @(DataFrame $t $ds)
      return $ conjoin
            [ index U df === df
            , indexOffset 0 df === df ]
      |]
  )

prop_IndexFrame1 :: Property
prop_IndexFrame1 = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeQ
    (as, bs) <- someLenAsBsQ 1
    [e| do
      i@(Idx ii :* U) <- arbitrary @(Idxs $as)
      df <- arbitrary @(DataFrame $t $(concatDimsQ as bs))
      return $ index i df === df ! ii
      |]
  )

prop_IndexFrame2 :: Property
prop_IndexFrame2 = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeQ
    (as, bs) <- someLenAsBsQ 2
    [e| do
      i@((Idx ii1) :* (Idx ii2) :* U) <- arbitrary @(Idxs $as)
      df <- arbitrary @(DataFrame $t $(concatDimsQ as bs))
      return $ index i df === df ! ii1 ! ii2
      |]
  )

prop_IndexOffset :: Property
prop_IndexOffset = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeQ
    (as, bs) <- someAsBsNQ
    [e| do
      i <- arbitrary @(Idxs $as)
      df <- arbitrary @(DataFrame $t $(concatDimsQ as bs))
      let asw = listDims (dims @($as))
          bsN = totalDim (dims @($bs))
          isw = listIdxs i
          off = fromIntegral . snd
              . foldr (\(dn, di) (tn, to) -> (tn*dn, to + tn*di) ) (bsN, 0)
              $ zip asw isw
      return $ index i df === indexOffset off df
      |]
  )

prop_Foldlr :: Property
prop_Foldlr = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeFQ
    (as, bs) <- someAsBsQ
    let asbs = concatDimsQ as bs
    [e| do
      df <- arbitrary @(DataFrame $t $asbs)
      let lim = ewfoldr' @($t) @($asbs) (max.abs) 1 df
              * ewfoldr' @($t) @($as) @($bs) (const (+1)) 0 df
          l = ewfoldl' (+) 10 df
          r = ewfoldr' (+) 0 df + 10 :: DataFrame $t $bs
      return $ approxEq lim l r
      |]
  )

prop_LazyFoldlr :: Property
prop_LazyFoldlr = property $ oneof $( aFewPropVariants $ do
    t <- someElemTypeQ
    (as, bs) <- someAsBsQ
    let asbs = concatDimsQ as bs
    [e| do
      n <- fromIntegral <$> choose (0, maxTotalDim `quot` 2)
      df <- arbitrary @(DataFrame $t $asbs)
      let S td = ewfoldr' @($t) @($as) @($bs) (const (+1)) 0 df
          l  = ewfoldl  @($t) @($as) @($bs) (limitedList) (const []) df n
          l' = ewfoldl' @($t) @($as) @($bs) (flip (:)) [] df
          r  = ewfoldr  @($t) @($as) @($bs) (flip limitedList) (const []) df n
          r' = ewfoldr' @($t) @($as) @($bs) (:) [] df

      return $
        counterexample
        ( unlines
         [ "Failed with dims:"
         , "  n:   " ++ show n
         , "  td:  " ++ show td
         , "  @as: " ++ show (typeOf $ minimalDims @($as))
         , "  @bs: " ++ show (typeOf $ minimalDims @($bs))
         ]
        ) $ conjoin
          [ if n >= td
            then l === reverse r
            else shouldErr (l === l') .&&. shouldErr (r === r')
          , take n l === take n l'
          , take n r === take n r'
          ]
      |]
  ) where
      shouldErr :: Property -> Property
      shouldErr p = ioProperty $ checkErr
        <$> quickCheckWithResult stdArgs { chatty = False }  p
      checkErr :: Result -> Property
      checkErr Failure { theException = Just _}
        = property True
      checkErr _
        = counterexample "Should have failed with undefined." False
      limitedList :: (Int -> [a]) -> a -> Int -> [a]
      limitedList f a k | k <= 0    = undefined
                        | otherwise = a : f (k - 1)


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
