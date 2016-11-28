{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Types
import GHC.Prim

import Numeric.Tensor
import qualified Numeric.Vector as V

main :: IO ()
main = do
  putStrLn "Hello world!"
  print (two `plus` vec2 3 4)
  print (I# (0.0# `geFloat#` 1.0#))
  print (I# (1.0# `geFloat#` 1.0#))
  print (I# (2.0# `geFloat#` 1.0#))
--  print (1 .< 4 .< two)
  putStrLn "Done!"
  where
    two = vec2 2 2
