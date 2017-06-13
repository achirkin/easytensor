
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MagicHash                  #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits
import GHC.Types
--import Numeric.Tensor (Dim(..))
-- import           Numeric.Commons
-- import qualified Numeric.Tensor     as T
-- import Numeric.Vector ((<:>))
-- import qualified Numeric.Vector as V
-- import qualified Numeric.Matrix as M
-- import           Numeric.EasyTensor

-- import           Foreign.Marshal
-- import           Foreign.Ptr
-- import           Foreign.Storable
-- import Data.Type.Equality
import Numeric.Dimensions
import Numeric.DataFrame
import Numeric.Commons (ix)
-- import Unsafe.Coerce
-- import Data.Functor.Identity
-- import Data.Functor.Const
-- import Data.Semigroup


main :: IO ()
main = do
    print $ F# (ix 0# (3 :: Float))
    putStrLn "Hello world!"
    print (Proxy @3 :* Proxy @2 :* (D :: Dim ('[] :: [Nat])))
    print $ case (,,,)  <$> someNatVal 3
                        <*> someNatVal 6
                        <*> someNatVal 8
                        <*> someNatVal 4
                        of
      Nothing -> Nothing
      Just (a,b,c,d) -> xDimVal $ a :? b :? c :? d :? D
    print (fromList [vec2 1 0, vec2 2 3, vec2 3 4, vec2 5 6] :: DataFrame Int '[N 2, XN])
    print (fromList [vec4 1 0 2 11, vec4 2 22 3 0, vec4 3 4 0 0] :: DataFrame Double '[N 4, XN])
    print (fromList [vec2 0 0, vec2 2 22, vec2 2 22] :: DataFrame Float '[N 2, XN])
    print (fromList [0, 1, 3, 5, 7] :: DataFrame Float '[XN])
    print (fromList [9, 13, 2] :: DataFrame Float '[N 5, N 2, XN])
    print $ vec2 1 1 %* mat22 (vec2 1 1) (vec2 2 (3 :: Float))
    print (toList (42 :: DataFrame Int '[4,3,2]))
    -- Seems like I have to specify known dimension explicitly,
    -- because the inference process within the pattern match
    -- cannot escape the case expression.
    -- On the other hand, if I type wrong dimension it will throw a nice type-level error.
    () <- case fromList [10, 100, 1000] :: DataFrame Double '[N 4, N 2, XN] of
                    -- Amazing inference!
                    -- m :: KnownNat k => DataFrame '[4,2,k]
        SomeDataFrame m -> print $ vec4 1 2.25 3 0.162 %* m
    putStrLn "Constructing larger matrices"
    let x :: DataFrame Float '[2,5,4]
        x =   transpose ( (56707.4   <::> 73558.41  <+:> 47950.074  <+:> 83394.61  <+:> 25611.629)
                     <::> (53704.516 <::> -3277.478 <+:> 99479.92   <+:> 18915.17  <+:> 59666.938) )
         <::> transpose ( (-3035.543 <::> 15831.447 <+:> 73256.625  <+:> 80709.38  <+:> 72695.04 )
                     <::> (50932.49  <::> 7865.496  <+:> -4050.5957 <+:> 99839.41  <+:> 10834.297) )
         <+:> transpose ( (21961.227 <::> 29640.914 <+:> 39657.19   <+:> 81469.64  <+:> 17815.506 )
                     <::> (-8484.239 <::> 16877.531 <+:> 65145.742  <+:> 80219.67  <+:> 81508.87) )
         <+:> transpose ( (53105.71  <::> 16255.646 <+:> 23324.957  <+:> -4438.164 <+:> 35369.824 )
                     <::> (67930.45  <::> 8950.834  <+:> 64451.71   <+:> 76685.57  <+:> 6728.465) )
        y :: DataFrame Float '[3,7]
        y = transpose $
               (70096.85  <::> 34332.492 <+:> 3642.8867 <+:> 25242.25  <+:> 59776.234 <+:> 12092.57 <+:> 10708.498)
          <::> (46447.965 <::> 37145.668 <+:> 56899.656 <+:> 85367.56  <+:> 15872.262 <+:> 87466.24 <+:> 82506.76 )
          <+:> (50458.848 <::> 31650.453 <+:> 71432.78  <+:> 53073.203 <+:> 59267.883 <+:> 82369.89 <+:> 78171.56 )
        z = ewgen x :: DataFrame Float '[2,5,4,3,7]
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
                  (\(i:!Z) v -> fromScalar . (scalar i +) . round
                                     $ vec3 0.02 (-0.01) 0.001 %* v
                  ) y

    -- Using elementWise function we can apply arbitrary applicative functors
    -- over subtensors.
    -- This means we can even do arbitrary IO for each subtensor
    -- indexed by suffix dimensions.
    putStrLn "\nWelement-wise IO!"
    rVec <- elementWise @Float @_ @'[4] @_
              (\v -> print v >> return (sqrt . trace $ v %* transpose v)) x
    putStrLn "\nTraces for each matrix element:"
    print rVec
