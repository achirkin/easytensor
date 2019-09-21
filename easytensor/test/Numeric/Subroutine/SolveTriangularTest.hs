{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Subroutine.SolveTriangularTest (runTests) where


import Control.Monad.ST
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.DataFrame.ST
import Numeric.Dimensions
import Numeric.Subroutine.SolveTriangular
import Test.QuickCheck


arbitraryTriangular ::
       forall (n :: Nat) (m :: Nat)
     . (KnownDim n, KnownDim m, KnownBackend Double '[m])
    => Bool -> Gen (DataFrame Double '[n,m])
arbitraryTriangular upper = iwmap f <$> arbitrary
  where
    f :: Idxs '[n] -> DataFrame Double '[m] -> DataFrame Double '[m]
    f (Idx i :* U) x = iwmap g x
      where
        a = maxElem x
        g :: Idxs '[m] -> Scalar Double -> Scalar Double
        g (Idx j :* U) c
          | if upper then j < i else j > i = 0
          | j == i = if c >= 0 then c + 1 + a else c - 1 - a
          | otherwise = c

-- | Test Rx = b
testSolveUpperTriangularR ::
       forall (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (KnownDim n, KnownDim m, m <= n, Dimensions ds)
    => DataFrame Double '[n,m]     -- ^ R
    -> DataFrame Double  (n :+ ds) -- ^ b
    -> Property
testSolveUpperTriangularR r b
  | dn <- dim @n
  , dm <- dim @m
  , di@D <- dn `minusDim` dm `plusDim` D1
  , i0 <- (Idx 0 :* U) `inSpaceOf` (di :* U)
  , Just Dict <- sameDim (plusDim dn D1) (plusDim di dm)
    = let bm :: DataFrame Double (m :+ ds)
          bm = slice i0 b
          x :: DataFrame Double (m :+ ds)
          x = runST $ do
            xPtr <- thawDataFrame bm
            solveUpperTriangularR r xPtr
            unsafeFreezeDataFrame xPtr
      in counterexample
          (unlines
            [ "failed Rx = b:"
            , "R: " ++ show r
            , "x: " ++ show x
            , "b: " ++ show b
            , "Rx:" ++ show (r %* x)
            ]
          ) (approxEq (maxElem r) b (r %* x))
testSolveUpperTriangularR _ _ = error "impossible pattern"

prop_SolveUpperTriangularR :: Property
prop_SolveUpperTriangularR = property run
  where
    run :: Gen Property
    run = do
      Dx (m :: Dim m) <- arbitrary :: Gen (Dim (XN 1))
      Dx (D :: Dim n) <- arbitrary :: Gen (Dim (XN m))
      justVec         <- arbitrary :: Gen Bool
      SomeDims (Dims :: Dims ds)
                      <- if justVec
                         then pure (SomeDims U)
                         else removeDimsAbove 100 <$> arbitrary
      r <- arbitraryTriangular @n @m True
      b' <- arbitrary @(DataFrame Double (n :+ ds))
      let b = iwmap @_ @(n ': ds) @'[]
                (\(Idx i :* _) x -> if i >= dimVal m then 0 else x) b'
      return $ testSolveUpperTriangularR r b

-- | Test xR = b
testSolveUpperTriangularL ::
       forall (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (KnownDim n, KnownDim m, m <= n, Dimensions ds)
    => DataFrame Double  (ds +: m) -- ^ b
    -> DataFrame Double '[n,m] -- ^ r
    -> Property
testSolveUpperTriangularL b r
  | dn <- dim @n
  , dm <- dim @m
  , dnm@D  <- minusDim dn dm
  , Dims <- Snoc (dims @ds) dn
  , Dims <- Snoc (dims @ds) dm
  , Dict <- Dict @(SnocList ds n _)
  , Dict <- Dict @(SnocList ds m _)
  , Just Dict <- sameDim dn (plusDim dm dnm)
    = let padZeroes :: DataFrame Double (ds +: m) -> DataFrame Double (ds +: n)
          padZeroes = ewmap @_ @ds @'[n] $ \a -> appendDF a (0 `inSpaceOf` (dnm :* U))
          x :: DataFrame Double (ds +: n)
          x = runST $ do
            xPtr <- thawDataFrame b
            solveUpperTriangularL xPtr r
            padZeroes <$> unsafeFreezeDataFrame xPtr
      in  counterexample
            (unlines
              [ "failed xR = b:"
              , "R: " ++ show r
              , "x: " ++ show x
              , "b: " ++ show b
              , "xR:" ++ show (x %* r)
              ]
            ) (approxEq (maxElem r) b (x %* r))
testSolveUpperTriangularL _ _ = error "impossible pattern"

prop_SolveUpperTriangularL :: Property
prop_SolveUpperTriangularL = property run
  where
    run :: Gen Property
    run = do
      Dx (m :: Dim m) <- arbitrary :: Gen (Dim (XN 1))
      Dx (D :: Dim n) <- arbitrary :: Gen (Dim (XN m))
      justVec         <- arbitrary :: Gen Bool
      SomeDims (ds@Dims :: Dims ds)
                      <- if justVec
                         then pure (SomeDims U)
                         else removeDimsAbove 100 <$> arbitrary
      Dims <- pure $ Snoc ds m
      r <- arbitraryTriangular @n @m True
      b <- arbitrary @(DataFrame Double (ds +: m))
      return $ testSolveUpperTriangularL b r


-- | Test Lx = b
testSolveLowerTriangularR ::
       forall (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (KnownDim n, KnownDim m, n <= m, Dimensions ds)
    => DataFrame Double '[n,m]     -- ^ L
    -> DataFrame Double  (n :+ ds) -- ^ b
    -> Property
testSolveLowerTriangularR l b
  | dn <- dim @n
  , dm <- dim @m
  , dmn@D <- dm `minusDim` dn
  , Just Dict <- sameDim dm (dn `plusDim` dmn)
    = let padZeroes :: DataFrame Double (n :+ ds) -> DataFrame Double (m :+ ds)
          padZeroes z = appendDF z (0 :: DataFrame Double ((m-n) :+ ds))
          x :: DataFrame Double (m :+ ds)
          x = runST $ do
            xPtr <- thawDataFrame b
            solveLowerTriangularR l xPtr
            padZeroes <$> unsafeFreezeDataFrame xPtr
      in counterexample
          (unlines
            [ "failed Lx = b:"
            , "L: " ++ show l
            , "x: " ++ show x
            , "b: " ++ show b
            , "Lx:" ++ show (l %* x)
            ]
          ) (approxEq (maxElem l) b (l %* x))
testSolveLowerTriangularR _ _ = error "impossible pattern"

prop_SolveLowerTriangularR :: Property
prop_SolveLowerTriangularR = property run
  where
    run :: Gen Property
    run = do
      Dx (_ :: Dim n) <- arbitrary :: Gen (Dim (XN 1))
      Dx (_ :: Dim m) <- arbitrary :: Gen (Dim (XN n))
      justVec         <- arbitrary :: Gen Bool
      SomeDims (Dims :: Dims ds)
                      <- if justVec
                         then pure (SomeDims U)
                         else removeDimsAbove 100 <$> arbitrary
      l <- arbitraryTriangular @n @m False
      b <- arbitrary @(DataFrame Double (n :+ ds))
      return $ testSolveLowerTriangularR l b


-- | Test xL = b
testSolveLowerTriangularL ::
       forall (n :: Nat) (m :: Nat) (ds :: [Nat])
     . (KnownDim n, KnownDim m, n <= m, Dimensions ds)
    => DataFrame Double  (ds +: m) -- ^ b
    -> DataFrame Double '[n,m] -- ^ L
    -> Property
testSolveLowerTriangularL b l
  | dn <- dim @n
  , dm <- dim @m
  , dmn@D  <- minusDim dm dn
  , Dims <- Snoc (dims @ds) dn
  , Dims <- Snoc (dims @ds) dm
  , Dict <- Dict @(SnocList ds n _)
  , Dict <- Dict @(SnocList ds m _)
  , Just Dict <- sameDim dm (plusDim dn dmn)
  , di@D <- dm `minusDim` dn `plusDim` D1
  , i0 <- (Idx 0 :* U) `inSpaceOf` (di :* U)
  , Just Dict <- sameDim (plusDim dm D1) (plusDim di dn)
    = let dropLast :: DataFrame Double (ds +: m) -> DataFrame Double (ds +: n)
          dropLast = ewmap @_ @ds @'[n] $ slice i0
          x :: DataFrame Double (ds +: n)
          x = runST $ do
            xPtr <- thawDataFrame b
            solveLowerTriangularL xPtr l
            dropLast <$> unsafeFreezeDataFrame xPtr
      in  counterexample
            (unlines
              [ "failed xL = b:"
              , "R: " ++ show l
              , "x: " ++ show x
              , "b: " ++ show b
              , "xL:" ++ show (x %* l)
              ]
            ) (approxEq (maxElem l) b (x %* l))
testSolveLowerTriangularL _ _ = error "impossible pattern"

prop_SolveLowerTriangularL :: Property
prop_SolveLowerTriangularL = property run
  where
    run :: Gen Property
    run = do
      Dx (n :: Dim n) <- arbitrary :: Gen (Dim (XN 1))
      Dx (m :: Dim m) <- arbitrary :: Gen (Dim (XN n))
      justVec         <- arbitrary :: Gen Bool
      SomeDims (ds@Dims :: Dims ds)
                      <- if justVec
                         then pure (SomeDims U)
                         else removeDimsAbove 100 <$> arbitrary
      Dims <- pure $ Snoc ds m
      Dict <- pure $ Dict @(SnocList ds m _)
      l <- arbitraryTriangular @n @m False
      b' <- arbitrary @(DataFrame Double (ds +: m))
      let b = ewmap @_ @ds @'[m]
                (iwmap @_ @'[m] @'[]
                  (\(Idx i :* _) x -> if i >= dimVal n then 0 else x)
                ) b'
      return $ testSolveLowerTriangularL b l

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
