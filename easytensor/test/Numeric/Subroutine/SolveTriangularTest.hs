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


import           Control.Monad.ST
import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries
import           Numeric.DataFrame.ST
import           Numeric.Dimensions
import           Numeric.Subroutine.SolveTriangular
import qualified Numeric.TypedList                  as TypedList (concat)
import           Test.QuickCheck


eps :: Fractional t => Scalar t
eps = 0.00001

-- | Most of the time, the error is proportional to the maginutude of the biggest element
maxElem :: (SubSpace t ds '[] ds, Ord t, Num t)
        => DataFrame t (ds :: [Nat]) -> Scalar t
maxElem = ewfoldl (\a -> max a . abs) 0

approxEq ::
  forall t (ds :: [Nat]) .
  (
    Dimensions ds,
    Fractional t, Ord t, Show t,
    Num (DataFrame t ds),
    PrimBytes (DataFrame t ds),
    PrimArray t (DataFrame t ds)
  ) => DataFrame t ds -> DataFrame t ds -> Property
approxEq a b = counterexample
    (unlines
      [ "  approxEq failed:"
      , "    max rows: "   ++ show m
      , "    max diff: "   ++ show dif
      ]
    ) $ maxElem (a - b) <= eps * m
  where
    m = maxElem a `max` maxElem b
    dif = maxElem (a - b)
infix 4 `approxEq`


arbitraryTriangular ::
       forall (n :: Nat) (m :: Nat)
     . (KnownDim n, KnownDim m)
    => Bool -> Gen (DataFrame Double '[n,m])
arbitraryTriangular
  | Dict <- inferKnownBackend @_ @Double @'[m] = arbitraryTriangular' @n @m

arbitraryTriangular' ::
       forall (n :: Nat) (m :: Nat)
     . (KnownDim n, KnownDim m, KnownBackend Double '[m])
    => Bool -> Gen (DataFrame Double '[n,m])
arbitraryTriangular' upper = iwmap f <$> arbitrary
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
  | Dict <- inferKnownBackend @_ @Double @'[n]
  , Dict <- inferKnownBackend @_ @Double @'[m]
  , Dict <- inferKnownBackend @_ @Double @(n :+ ds)
  , Dict <- inferKnownBackend @_ @Double @(m :+ ds)
  , dn <- dim @n
  , dm <- dim @m
  , di@D <- dn `minusDim` dm `plusDim` D1
  , i0 <- (Idx 0 :* U) `inSpaceOf` (di :* U)
  , Just Dict <- minusDimM (di `plusDim` dm) D1 >>= sameDim dn
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
          ) (approxEq (r %* x) b)
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
      let b | Dict <- inferKnownBackend @_ @Double @(n :+ ds)
            = iwmap @_ @(n ': ds) @'[]
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
  , dsn1@Dims <- Snoc (dims @ds) dn
  , dsn2@Dims <- TypedList.concat (dims @ds) (dn :* U)
  , dsm1@Dims <- Snoc (dims @ds) dm
  , dsm2@Dims <- TypedList.concat (dims @ds) (dm :* U)
  , Just Dict <- sameDims dsm1 dsm2
  , Just Dict <- sameDims dsn1 dsn2
  , Dict <- inferConcat @Nat @ds @'[m] @(ds +: m)
  , Dict <- inferConcat @Nat @ds @'[n] @(ds +: n)
  , Just Dict <- sameDim dn (plusDim dm dnm)
  , Dict <- inferKnownBackend @_ @Double @'[n]
  , Dict <- inferKnownBackend @_ @Double @'[m]
  , Dict <- inferKnownBackend @_ @Double @'[n - m]
  , Dict <- inferKnownBackend @_ @Double @(ds +: n)
  , Dict <- inferKnownBackend @_ @Double @(ds +: m)
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
            ) (approxEq (x %* r) b)
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
  | Dict <- inferKnownBackend @_ @Double @'[n]
  , Dict <- inferKnownBackend @_ @Double @'[m]
  , Dict <- inferKnownBackend @_ @Double @(n :+ ds)
  , Dict <- inferKnownBackend @_ @Double @(m :+ ds)
  , dn <- dim @n
  , dm <- dim @m
  , dmn@D <- dm `minusDim` dn
  , Dict <- inferKnownBackend @_ @Double @((m-n) :+ ds)
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
          ) (approxEq (l %* x) b)
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
  , dsn1@Dims <- Snoc (dims @ds) dn
  , dsn2@Dims <- TypedList.concat (dims @ds) (dn :* U)
  , dsm1@Dims <- Snoc (dims @ds) dm
  , dsm2@Dims <- TypedList.concat (dims @ds) (dm :* U)
  , Just Dict <- sameDims dsm1 dsm2
  , Just Dict <- sameDims dsn1 dsn2
  , Dict <- inferConcat @Nat @ds @'[m] @(ds +: m)
  , Dict <- inferConcat @Nat @ds @'[n] @(ds +: n)
  , Just Dict <- sameDim dm (plusDim dn dmn)
  , Dict <- inferKnownBackend @_ @Double @'[n]
  , Dict <- inferKnownBackend @_ @Double @'[m]
  , Dict <- inferKnownBackend @_ @Double @'[m - n]
  , Dict <- inferKnownBackend @_ @Double @(ds +: n)
  , Dict <- inferKnownBackend @_ @Double @(ds +: m)
  , di@D <- dm `minusDim` dn `plusDim` D1
  , i0 <- (Idx 0 :* U) `inSpaceOf` (di :* U)
  , Just Dict <- minusDimM (di `plusDim` dn) D1 >>= sameDim dm
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
            ) (approxEq (x %* l) b)
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
      dsm1@Dims <- pure $ Snoc ds m
      dsm2@Dims <- pure $ TypedList.concat ds (m :* U)
      Just Dict <- pure $ sameDims dsm1 dsm2
      Dict <- pure $ inferConcat @Nat @ds @'[m] @(ds +: m)
      Dict <- pure $ inferKnownBackend @_ @Double @(ds +: m)
      Dict <- pure $ inferKnownBackend @_ @Double @'[m]
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
