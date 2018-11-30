{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.Semigroup
import Numeric.DataFrame.Type
import Numeric.DataFrame.Internal.Array.Family


main :: IO ()
main = do
    print $ DF UnitBase <> (mempty :: DataFrame Double 0)
    print $ DF (ScalarBase (7 :: Double)) <> DF (ScalarBase 15)
    print $ DF (ScalarBase (7 :: Double)) <> mempty
    print $ mempty <> DF (Vec2Base 2 6) <> (DF (Vec2Base 3 12) :: DataFrame Double 2)
    print $ mempty <> DF (ListBase [9,8,7,6,5]) <> (DF (ListBase [1,2,3,4,5]) :: DataFrame Double 5)
    case sdf2 of
      SomeDataFrame x -> print $ mappend x x <> mempty
    case sdf7 of
      SomeDataFrame x -> print $ f x
  where
    sdf2 = SomeDataFrame $ DF (Vec2Base 2 (6 :: Int))
    sdf7 = SomeDataFrame
      (DF (ListBase [1,2,3,4,5,16,92]) :: DataFrame Float 7)



f :: ( Semigroup t, Monoid t) => t -> t
f x = x <> x <> x <> mempty <> x
{-
 Pragma NOINLINE reduces the number of calls to the dictionary function.
 With optimization enabled, this is 6 vs 3.
 Assuming one call is for Show instance, f invokes the DFun  once for each type.

 If the function is inlined,  DFun seems to be invoked every time the Monoid
 or Semigroup functions are called.
 -}
{-# NOINLINE f #-}
