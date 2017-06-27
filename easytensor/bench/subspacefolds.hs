{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main (main) where

import           Data.Time.Clock

import           Numeric.DataFrame
import           Numeric.Dimensions


type DList = [6,8,10,7,35,8,12] -- [6,26,8,10,35,8,12]

main :: IO ()
main = do
    t0 <- getCurrentTime
    putStrLn $ "\nStarting benchmarks, current time is " ++ show t0
    let df = iwgen @Float @'[] @DList (fromIntegral . fromEnum)
    t1 <- df `seq` getCurrentTime
    seq t1 putStrLn $ "Created DataFrame, elapsed time is " ++ show (diffUTCTime t1 t0)

    putStrLn "\nRunning a ewfoldl on scalar elements..."
    let rezEwf = ewfoldl @Float @'[] @DList (\a x -> a + a / (x+1)) 1 df
    t2 <- rezEwf `seq` getCurrentTime
    print rezEwf
    seq t2 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t2 t1)

    putStrLn "\nRunning a iwfoldl on scalar elements (not using idx)..."
    let rezIwf = iwfoldl @Float @'[] @DList (\_ a x -> a + a / (x+1)) 1 df
    t3 <- rezIwf `seq` getCurrentTime
    print rezIwf
    seq t3 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t3 t2)

    putStrLn "\nRunning a iwfoldl on scalar elements (using fromEnum idx)..."
    let rezIwf2 = iwfoldl @Float @'[] @DList (\i a x -> a + x / ((1+) . fromIntegral $ fromEnum i)) 0 df
    t4 <- rezIwf2 `seq` getCurrentTime
    print rezIwf2
    seq t4 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t4 t3)

    putStrLn "\nRunning a iwfoldl on scalar elements (enforcing idx)..."
    let rezIwf3 = iwfoldl @Float @'[] @DList (\i a x -> i `seq` a + a / (x+1)) 1 df
    t5 <- rezIwf3 `seq` getCurrentTime
    print rezIwf3
    seq t5 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t5 t4)

    putStrLn "\nRunning a ewfoldl on vector5 elements..."
    let rezEwv1 = ewfoldl @Float @'[Head DList] @(Tail DList)
                          (\a x -> a + a / (1 + iwgen @_ @'[] (\(i:!Z) -> (i+1):!Z !. x )) )
                          (3 :: DataFrame Float '[5]) df
    t6 <- rezEwv1 `seq` getCurrentTime
    print rezEwv1
    seq t6 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t6 t5)

    putStrLn "\nRunning a ewfoldl on vector3 elements..."
    let rezEwv2 = ewfoldl @Float @'[Head DList] @(Tail DList)
                          (\a x -> a + a / (1 + iwgen @_ @'[] (\(i:!Z) -> (i+1):!Z !. x )) )
                          (3 :: DataFrame Float '[3]) df
    t7 <- rezEwv2 `seq` getCurrentTime
    print rezEwv2
    seq t7 putStrLn $ "Done; elapsed time = " ++ show (diffUTCTime t7 t6)


    putStrLn "Checking indexes"
    print $ 1:!1:!3:!1:!Z !. df
