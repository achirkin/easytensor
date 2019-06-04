{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main (main) where

import Numeric.DataFrame
import Numeric.Dimensions

import qualified Control.Monad.ST     as ST
import qualified Numeric.DataFrame.ST as ST



main :: IO ()
main = do
    putStrLn "Hello world!"
    print (D @3 :* D @2 :* U)

    print (fromList D [vec2 1 0, vec2 2 3, vec2 3 4, vec2 5 6]
             :: Maybe (DataFrame Int '[XN 2, N 2]))
    print (fromList D [vec4 1 0 2 11, vec4 2 22 3 0, vec4 3 4 0 0]
             :: Maybe (DataFrame Double '[XN 0, N 4]))
    print (fromList D [vec2 0 0, vec2 2 22, vec2 2 22]
             :: Maybe (DataFrame Float '[XN 4, N 2]))
    print $ fromList (D @3) [0 :: Scf, 1, 3, 5, 7]
    print ( fromList D [9, 13, 2]
             :: Maybe (DataFrame Float '[XN 2, N 5, N 2]))
    print $ vec2 1 1 %* mat22 1 (vec2 2 (3 :: Float))
    print (toList (42 :: DataFrame Int '[4,3,2]))
    -- Seems like I have to specify known dimension explicitly,
    -- because the inference process within the pattern match
    -- cannot escape the case expression.
    -- On the other hand, if I type wrong dimension it will throw a nice type-level error.
    () <- case fromList D [10, 100, 1000] :: Maybe (DataFrame Double '[XN 2, N 2, N 4]) of
        Just (XFrame m)
          | KnownDims <- dims `inSpaceOf` m
              -- Amazing inference!
              -- m :: KnownNat k => DataFrame '[k,2,4]
            -> print $ m %* vec4 1 2.25 3 0.162
        _ -> print "Failed to construct a DataFrame!"
    putStrLn "Constructing larger matrices"
    let x :: DataFrame Double '[4,5,2]
        x =   transpose ( (56707.4   <::> 73558.41  <+:> 47950.074  <+:> 83394.61  <+:> 25611.629 )
                     <::> (53704.516 <::> -3277.478 <+:> 99479.92   <+:> 18915.17  <+:> 59666.938 ) )
         <::> transpose ( (-3035.543 <::> 15831.447 <+:> 73256.625  <+:> 80709.38  <+:> 72695.04  )
                     <::> (50932.49  <::> 7865.496  <+:> -4050.5957 <+:> 99839.41  <+:> 10834.297 ) )
         <+:> transpose ( (21961.227 <::> 29640.914 <+:> 39657.19   <+:> 81469.64  <+:> 17815.506 )
                     <::> (-8484.239 <::> 16877.531 <+:> 65145.742  <+:> 80219.67  <+:> 81508.87  ) )
         <+:> transpose ( (53105.71  <::> 16255.646 <+:> 23324.957  <+:> -4438.164 <+:> 35369.824 )
                     <::> (67930.45  <::> 8950.834  <+:> 64451.71   <+:> 76685.57  <+:> 6728.465  ) )
        y :: DataFrame Double '[7,3]
        y = transpose $
               (70096.85  <::> 34332.492 <+:> 3642.8867 <+:> 25242.25  <+:> 59776.234 <+:> 12092.57 <+:> 10708.498)
          <::> (46447.965 <::> 37145.668 <+:> 56899.656 <+:> 85367.56  <+:> 15872.262 <+:> 87466.24 <+:> 82506.76 )
          <+:> (50458.848 <::> 31650.453 <+:> 71432.78  <+:> 53073.203 <+:> 59267.883 <+:> 82369.89 <+:> 78171.56 )
        z = ewgen x :: DataFrame Double '[7,3,4,5,2]
    print $ ewfoldl @_ @_ @'[2] (+) 10 z
    print $ ewfoldr @_ @_ @'[5,2] (+) 0 z + 10
    print $ ewfoldl (+) 10 z - ewfoldr @_ @_ @'[4,5,2] (+) 0 z - 10

    -- We can map arbitrary suffix dimension over the dataframe,
    -- indexing by prefix dimensions.
    -- At the same time, we can transform underlying element type
    --  or suffix dimensionality.
    -- For example, we can do tensor produt of every sub-tensor.
    putStrLn "\nConversions between element types and frame sizes."
    print $ iwmap @Int @'[7] @'[2,2] @_
                  (\(i:*U) v -> fromScalar . (scalar (fromIntegral i) +) . round
                                     $ vec3 0.02 (-0.01) 0.001 %* v
                  ) y

    -- Using elementWise function we can apply arbitrary applicative functors
    -- over subtensors.
    -- This means we can even do arbitrary IO for each subtensor
    -- indexed by suffix dimensions.
    putStrLn "\nWelement-wise IO!"
    rVec <- elementWise @Double @'[4] @_  @_
              (\v -> print v >> return (sqrt . trace $ v %* transpose v)) x
    putStrLn "\nTraces for each matrix element:"
    print rVec

    -- Updating existing frames
    print $ update (2:*U) (scalar 777) rVec
    print $ update (1:*3:*U) (vec2 999 555) x

    let matX = iwgen (scalar . fromEnum) :: DataFrame Int '[2,6,4]
        matY = iwgen (scalar . fromEnum) :: DataFrame Int '[5,4]
    putStrLn "Check carefully that this returns no garbage"
    print matX
    print (ewmap @_ @'[2,6] (<+:> scalar 111) matX :: DataFrame Int '[2,6,5])
    print matY
    print (ewmap fromScalar matY :: DataFrame Int '[5,4,3])

    -- Working with mutable frames
    print $ ST.runST $ do
      sdf <- ST.thawDataFrame matY
      ST.writeDataFrame sdf (0:*0:*U) 900101
      ST.writeDataFrame sdf (2:*2:*U) 900303
      ST.writeDataFrame sdf (4:*2:*U) 900503
      ST.unsafeFreezeDataFrame sdf
