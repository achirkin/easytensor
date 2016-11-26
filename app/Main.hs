module Main where

import Data.Tensor

main :: IO ()
main = do
  putStrLn $ show (vec2 1 2 `plus` vec2 3 4)
  print (1 .< 4 .< two)
  where
    two = vec2 2 2
