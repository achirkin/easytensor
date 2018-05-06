{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main (main) where

import           Numeric.DataFrame
import           Numeric.Dimensions

import qualified Control.Monad.ST as ST
import qualified Numeric.DataFrame.ST as ST
-- import qualified Numeric.Dimensions.Traverse.ST as ST



main :: IO ()
main = do
    putStrLn "Hello world!"
    print (D @3 :* D @2 :* U)

    print (fromList D [vec2 1 0, vec2 2 3, vec2 3 4, vec2 5 6]
             :: Maybe (DataFrame Int '[N 2, XN 2]))
    print (fromList D [vec4 1 0 2 11, vec4 2 22 3 0, vec4 3 4 0 0]
             :: Maybe (DataFrame Double '[N 4, XN 0]))
    print (fromList D [vec2 0 0, vec2 2 22, vec2 2 22]
             :: Maybe (DataFrame Float '[N 2, XN 4]))
    print $ fromList (D @3) [0 :: Scf, 1, 3, 5, 7]
    print ( fromList D [9, 13, 2]
             :: Maybe (DataFrame Float '[N 5, N 2, XN 2]))
    print $ vec2 1 1 %* mat22 1 (vec2 2 (3 :: Float))
    print (toList (42 :: DataFrame Int '[4,3,2]))
    -- TODO: FIX THIS
    -- -- Seems like I have to specify known dimension explicitly,
    -- -- because the inference process within the pattern match
    -- -- cannot escape the case expression.
    -- -- On the other hand, if I type wrong dimension it will throw a nice type-level error.
    -- () <- case fromList D [10, 100, 1000] :: Maybe (DataFrame Double '[N 4, N 2, XN 2]) of
    --                 -- Amazing inference!
    --                 -- m :: KnownNat k => DataFrame '[4,2,k]
    --     Just (XFrame m) -> print $ vec4 1 2.25 3 0.162 %* m
    --     Nothing -> print "Failed to construct a DataFrame!"
    putStrLn "Constructing larger matrices"
    let x :: DataFrame Double '[2,5,4]
        x =   transpose ( (56707.4   <::> 73558.41  <+:> 47950.074  <+:> 83394.61  <+:> 25611.629 )
                     <::> (53704.516 <::> -3277.478 <+:> 99479.92   <+:> 18915.17  <+:> 59666.938 ) )
         <::> transpose ( (-3035.543 <::> 15831.447 <+:> 73256.625  <+:> 80709.38  <+:> 72695.04  )
                     <::> (50932.49  <::> 7865.496  <+:> -4050.5957 <+:> 99839.41  <+:> 10834.297 ) )
         <+:> transpose ( (21961.227 <::> 29640.914 <+:> 39657.19   <+:> 81469.64  <+:> 17815.506 )
                     <::> (-8484.239 <::> 16877.531 <+:> 65145.742  <+:> 80219.67  <+:> 81508.87  ) )
         <+:> transpose ( (53105.71  <::> 16255.646 <+:> 23324.957  <+:> -4438.164 <+:> 35369.824 )
                     <::> (67930.45  <::> 8950.834  <+:> 64451.71   <+:> 76685.57  <+:> 6728.465  ) )
        y :: DataFrame Double '[3,7]
        y = transpose $
               (70096.85  <::> 34332.492 <+:> 3642.8867 <+:> 25242.25  <+:> 59776.234 <+:> 12092.57 <+:> 10708.498)
          <::> (46447.965 <::> 37145.668 <+:> 56899.656 <+:> 85367.56  <+:> 15872.262 <+:> 87466.24 <+:> 82506.76 )
          <+:> (50458.848 <::> 31650.453 <+:> 71432.78  <+:> 53073.203 <+:> 59267.883 <+:> 82369.89 <+:> 78171.56 )
        z = ewgen x :: DataFrame Double '[2,5,4,3,7]
    print $ ewfoldl @_ @'[2] (+) 10 z
    print $ ewfoldr @_ @'[2,5] (+) 0 z + 10
    print $ ewfoldl (+) 10 z - ewfoldr @_ @'[2,5,4] (+) 0 z - 10

    -- We can map arbitrary prefix dimension over the dataframe,
    -- indexing by suffix dimensions.
    -- At the same time, we can transform underlying element type
    --  or prefix dimensionality.
    -- For example, we can do tensor produt of every sub-tensor.
    putStrLn "\nConversions between element types and frame sizes."
    print $ iwmap @Int @'[2,2] @'[7] @_
                  (\(Idx i:*U) v -> fromScalar . (scalar (fromIntegral i) +) . round
                                     $ vec3 0.02 (-0.01) 0.001 %* v
                  ) y

    -- Using elementWise function we can apply arbitrary applicative functors
    -- over subtensors.
    -- This means we can even do arbitrary IO for each subtensor
    -- indexed by suffix dimensions.
    putStrLn "\nWelement-wise IO!"
    rVec <- elementWise @Double @_ @'[4] @_
              (\v -> print v >> return (sqrt . trace $ v %* transpose v)) x
    putStrLn "\nTraces for each matrix element:"
    print rVec

    -- Updating existing frames
    print $ update (2:*U) (scalar 777) rVec
    print $ update (2:*3:*U) (vec2 999 999) x

    let matX = iwgen (scalar . fromEnum) :: DataFrame Int '[2,5,4]
        matY = iwgen (scalar . fromEnum) :: DataFrame Int '[5,4]
    putStrLn "Check carefully that this returns no garbage"
    print matX
    print (ewmap (<+:> scalar 111) matX :: DataFrame Int '[3,5,4])
    print matY
    print (ewmap fromScalar matY :: DataFrame Int '[3,5,4])

    -- Working with mutable frames
    print $ ST.runST $ do
      sdf <- ST.thawDataFrame matY
      ST.writeDataFrame sdf (1:*1:*U) 900101
      ST.writeDataFrame sdf (3:*3:*U) 900303
      ST.writeDataFrame sdf (5:*3:*U) 900503
      ST.unsafeFreezeDataFrame sdf
