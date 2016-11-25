{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
module Main where

import Data.Tensor

main :: IO ()
main = putStrLn $ show (Tensor :: Tensor Double '[1,4,2] '[2,4,2])
