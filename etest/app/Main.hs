{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Data.Semigroup
import Numeric.DataFrame.Type
import Numeric.DataFrame.Internal.Array.Family


main :: IO ()
main = do
    print $ DataFrame UnitBase <> (mempty :: DataFrame Double 0)
    print $ DataFrame (ScalarBase (7 :: Double)) <> DataFrame (ScalarBase 15)
    print $ DataFrame (ScalarBase (7 :: Double)) <> mempty
    print $ mempty <> DataFrame (Vec2Base 2 6) <> (DataFrame (Vec2Base 3 12) :: DataFrame Double 2)
    print $ mempty <> DataFrame (ListBase [9,8,7,6,5]) <> (DataFrame (ListBase [1,2,3,4,5]) :: DataFrame Double 5)
    case sdf2 of
      SomeDataFrame x -> print $ mappend x x <> mempty
    case sdf7 of
      SomeDataFrame x -> print $ f x
  where
    sdf2 = SomeDataFrame $ DataFrame (Vec2Base 2 (6 :: Int))
    sdf7 = SomeDataFrame
      (DataFrame (ListBase [1,2,3,4,5,16,92]) :: DataFrame Float 7)



f :: ( Semigroup t, Monoid t) => t -> t
f x = x <> x <> x <> mempty <> x
