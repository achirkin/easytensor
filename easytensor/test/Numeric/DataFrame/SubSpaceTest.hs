-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.DataFrame.SubSpaceTest
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import           Numeric.DataFrame
import           Numeric.DataFrame.Arbitraries ()
import           Numeric.Dimensions
import           Test.QuickCheck



-- prop_Eye :: SomeDataFrame Double -> Bool
-- prop_Eye (SomeDataFrame (x :: DataFrame Double ds))
--   | Cons (D :: Dim dh) (Dims :: Dims dt) <- dims `inSpaceOf` x
--   , Snoc (Dims :: Dims di) (D :: Dim dl) <- dims `inSpaceOf` x
--   -- | Just Evidence <- sumEvs <$> inferUnConsDimensions @ds
--   --                           <*> inferUnSnocDimensions @ds
--     = (eye :: DataFrame Double '[dh, dh]) %* x == x
--     -- && x == x %* (eye :: DataFrame Double '[dh, dh])
--   -- | otherwise = False


prop_IndexDimMax :: DataFrame Int '[2,5,4] -> DataFrame Int '[3,7] -> Bool
prop_IndexDimMax x y =
   ((maxBound `inSpaceOf` y) !. z) == x
  where
    z = ewgen x :: DataFrame Int '[2,5,4,3,7]

prop_IndexCustom1 :: DataFrame Word '[2,5,4] -> DataFrame Word '[3,7] -> Bool
prop_IndexCustom1 x _ = (1:*3 !. z) == x
  where
    z = ewgen x :: DataFrame Word '[2,5,4,3,7]


prop_IndexCustom2 :: DataFrame Double '[2,5,4] -> DataFrame Double '[3,7] -> Bool
prop_IndexCustom2 x _ = (2:*2 !. z) %* eye == x
  where
    z = ewgen x :: DataFrame Double '[2,5,4,3,7]

-- TODO: following never finishes!
-- prop_Foldlr :: DataFrame Double '[2,5,4] -> DataFrame Double '[3,7] -> Bool
-- prop_Foldlr x _ =
--    abs (ewfoldl (+) 10 z - ewfoldr @_ @'[2,5,4] (+) 0 z - 10) <= fromScalar (zmax * 0.0001)
--   where
--     z = ewgen x :: DataFrame Double '[2,5,4,3,7]
--     zmax = ewfoldl @Double @'[] @'[2,5,4,3,7] (max . abs) 0.001 z

prop_Ewmap :: DataFrame Float '[2,5,4] -> DataFrame Float '[3,7] -> Bool
prop_Ewmap _ y =
   y * 2 == ewmap @_ @'[3] (*2) y


return []
runTests :: IO Bool
runTests = $quickCheckAll
