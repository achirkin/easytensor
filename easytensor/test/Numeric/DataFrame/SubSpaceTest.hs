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

type SFull = '[2,5,4,3,7]
type SPref = '[2,5,4]
type SSuff = '[3,7]

prop_IndexDimMax :: DataFrame Int SPref -> DataFrame Int SSuff -> Bool
prop_IndexDimMax x y =
   ((maxBound `inSpaceOf` y) !. z) == x
  where
    z = ewgen x :: DataFrame Int SFull

prop_IndexCustom1 :: DataFrame Word SPref -> Bool
prop_IndexCustom1 x = (1:*3 !. z) == x
  where
    z = ewgen x :: DataFrame Word SFull


prop_IndexCustom2 :: DataFrame Double SPref -> Bool
prop_IndexCustom2 x = (2:*2 !. z) %* eye == x
  where
    z = ewgen x :: DataFrame Double SFull

prop_Foldlr :: DataFrame Double SPref -> Bool
prop_Foldlr x =
    abs (ewfoldl (+) 10 z - ewfoldr @_ @SPref (+) 0 z - 10)
      <= fromScalar (zmax * 0.0001)
  where
    z = ewgen x :: DataFrame Double SFull
    zmax = ewfoldl @Double @'[] @SFull (max . abs) 0.001 z

prop_Ewmap :: DataFrame Double SFull -> Bool
prop_Ewmap x = x * 2 == ewmap @_ @'[Head SFull] (*2) x

prop_ProdTranspose :: DataFrame Double '[2,6] -> DataFrame Double '[6,7] -> Bool
prop_ProdTranspose x y = transpose (x %* y) == transpose y %* transpose x

prop_Eye :: DataFrame Double SFull -> Bool
prop_Eye x = eye %* x == x && x %* eye == x

return []
runTests :: IO Bool
runTests = $quickCheckAll
