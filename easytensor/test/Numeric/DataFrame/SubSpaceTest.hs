{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.DataFrame.SubSpaceTest (runTests) where

import Numeric.Arbitraries
import Numeric.DataFrame
import Numeric.Dimensions
import Test.QuickCheck

type SFull = '[2,5,4,3,7]
type SPref = '[2,5,4]
type SSuff = '[3,7]

prop_IndexDimMax :: DataFrame Int SPref -> DataFrame Int SSuff -> Bool
prop_IndexDimMax x y =
   (z .! (maxBound `inSpaceOf` x)) == y
  where
    z = ewgen y :: DataFrame Int SFull

prop_IndexCustom1 :: DataFrame Word SSuff -> Bool
prop_IndexCustom1 x = (z .! 1:*3:*2) == x
  where
    z = ewgen x :: DataFrame Word SFull

-- TODO:!
-- prop_IndexCustom2 :: DataFrame Double SSuff -> Bool
-- prop_IndexCustom2 x = (z ! 2:*2:*1) %* eye == x
--   where
--     z = ewgen x :: DataFrame Double SFull

prop_Foldlr :: DataFrame Double SSuff -> Property
prop_Foldlr x = approxEq zmax
    (ewfoldl' (+) 10 z) (ewfoldr' @_ @SPref (+) 0 z + 10)
  where
    z = ewgen x :: DataFrame Double SFull
    S zmax = ewfoldl' @Double @SFull @'[] (max . abs) 1 z

prop_Ewmap :: DataFrame Double SFull -> Bool
prop_Ewmap x = x * 2 == ewmap @_  @_ @'[Last SFull] (*2) x

prop_ProdTranspose :: DataFrame Double '[2,6] -> DataFrame Double '[6,7] -> Bool
prop_ProdTranspose x y = transpose (x %* y) == transpose y %* transpose x

prop_Eye :: DataFrame Double SFull -> Bool
prop_Eye x = eye %* x == x && x %* eye == x

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
