{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.Matrix.LUTest (runTests) where


import Control.Monad       (join)
import Data.Kind
import Data.Monoid         (All (..))
import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Matrix.LU
import Test.QuickCheck


validateLU :: forall (t :: Type) (n :: Nat)
             . ( KnownDim n, RealFloatExtras t, Show t
               , MatrixDeterminant t n)
            => Matrix t n n -> LU t n -> Property
validateLU x lux@LU {..} =
    counterexample
      (unlines
        [ "failed luPerm m =~= luLower %* luUpper:"
        , "x:   " ++ show x
        , "px:  " ++ show px
        , "px': " ++ show px'
        , "lu:  " ++ show lux
        ]
      ) (px =~= px')
    .&&.
    counterexample
      (unlines
        [ "Failed an extra invariant det luPerm == luPermDet:"
        , "lu:" ++ show lux
        ]
      ) (luPermDet =~= det luPerm)
    .&&.
    counterexample
      (unlines
        [ "luUpper is not upper-triangular"
        , "lu:" ++ show lux
        , "x:"  ++ show x
        ]
      ) (getAll $ iwfoldMap @t @'[n,n]
            (\(Idx i :* Idx j :* U) a -> All (j >= i || a == 0))
            luUpper
        )
    .&&.
    counterexample
      (unlines
        [ "luLower is not unit lower triangular,"
            ++ " or some of its elements are greater than one"
        , "lu:" ++ show lux
        , "x:"  ++ show x
        ]
      ) (getAll $ iwfoldMap @t @'[n,n]
            (\(Idx i :* Idx j :* U) a -> case compare i j of
                GT -> All (abs a <= 1)
                EQ -> All (a == 1)
                LT -> All (a == 0)
            ) luLower
        )
  where
    px  = luPerm %* x
    px' = luLower %* luUpper


manualMats :: forall t . (PrimBytes t, Num t) => [SomeSquareMatrix AnyMatrix t]
manualMats = join
      [ [mkM D2, mkM D4, mkM D9]
            <*> [repeat 0, repeat 1, repeat 2]
      , mkM D2 <$> variants [3,2, 4,1]
      , mkM D3 <$> variants [0,0,1, 3,2,0, 4,1,0]
      , mkM D2 <$> variants [2, 0, 0, 0]
      , mkM D2 <$> variants [4, 1, 0, 0]
      , mkM D2 <$> variants [3, 1, -2, 0]
      ]
  where
    mkM :: Dim n -> [t] -> SomeSquareMatrix AnyMatrix t
    mkM (d@D :: Dim n) = SSM . fromFlatList (d :* d :* U) 0
    mkM _              = error "manualMats: bad dims"
    variants :: Num a => [a] -> [[a]]
    variants as = rotateList as ++ rotateList (map negate as)


prop_luSimple :: Property
prop_luSimple = once . conjoin $ map prop_lu manualMats

prop_lu :: SomeSquareMatrix AnyMatrix Double -> Property
prop_lu (SSM x) = validateLU x (lu x)


return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
