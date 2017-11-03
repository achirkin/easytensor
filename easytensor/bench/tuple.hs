module Main (main) where

import Data.Time.Clock
import Numeric.Semigroup
import Numeric.Tuple


main :: IO ()
main = do
    let n = 10000000 :: Int
        one = 1 :: Int
    bench "MaxTup0Fold" $
      fromTuple $ foldMap' (const T0) [1..n]
    bench "MaxFold" $
      foldMap' Max [1..n]
    bench "MaxTupFold" $
      foldMap' (T1 <$> Max) [1..n]
    bench "MaxSumFold" $
      fromTuple $ foldMap' (T2 <$> Max <*> Sum) [1..n]
    bench "MaxSumMinFold" $
      fromTuple $ foldMap' (T3 <$> Max <*> Sum <*> Min) [1..n]
    bench "MaxSumMinMinMaxFold" $
      fromTuple $ foldMap' (T4 <$> Max <*> Sum <*> Min <*> minMax) [1..n]
    bench "MaxSumMinMinMaxAllFold" $
      fromTuple $ foldMap' (T5 <$> Max
                               <*> Sum . (`mod` 19)
                               <*> Min . (`mod` 23)
                               <*> minMax
                               <*> (All . (>0))
                            ) [1..n]
    bench "MaxSumMinMinMaxAllCountFold" $
      fromTuple $ foldMap' (T6 <$> Max
                               <*> Sum . (`mod` 19)
                               <*> Min . (`mod` 23)
                               <*> minMax
                               <*> (All . (>0))
                               <*> (Sum . const one)
                            ) [1..n]
    bench "MaxSumMinMinMaxAllCountProdFold" $
      fromTuple $ foldMap' (T7 <$> Max
                               <*> Sum . (`mod` 19)
                               <*> Min . (`mod` 23)
                               <*> minMax
                               <*> (All . (>0))
                               <*> (Sum . const one)
                               <*> Product . (+1) . (*(-2)) . (`mod` 2)
                            ) [1..n]




bench :: Show a => String -> a -> IO ()
bench bname v = do
    t0 <- getCurrentTime
    t1 <- t0 `seq` v `seq` putStrLn ("Evaluated " <> bname <> ":") >> print v >> getCurrentTime
    seq t1 putStrLn $ "Execution time: " <> show (diffUTCTime t1 t0)
    putStrLn ""
