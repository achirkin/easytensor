
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Proxy
import GHC.TypeLits
-- import           GHC.Exts
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
import Data.Type.Equality
import           Numeric.Dimensions
import Numeric.DataFrame
import Unsafe.Coerce

main :: IO ()
main = do
    putStrLn "Hello world!"
    print $ (Proxy @3 :* Proxy @2 :* (D :: Dim ('[] :: [Nat])))
    print $ case (,,,) <$> someNatVal 3
                                    <*> someNatVal 6
                                    <*> someNatVal 8
                                    <*> someNatVal 4
                                    of
      Nothing -> Nothing
      Just (a,b,c,d) -> someDimVal $ a :? b :? c :? d :? D
    print s
  where
    Just d2 = someNatVal 2
    Just d3 = someNatVal 5
    dimX :: Dim '[N 3, XN, XN]
    dimX = Proxy :* d2 :? d3 :? D
    s = withDim dimX (\ds -> case as3 ds of
                        (Refl, Refl) -> show (dfFloat pi `inSpaceOf` ds)
                     )
      :: Either String String

-- dfFloat :: (Fractional (DataFrame Float ds), Show (DataFrame Float ds))
--         => Float -> DataFrame Float (ds :: [Nat])
dfFloat :: Fractional (DataFrame Float (d ': ds))
        => Float -> DataFrame Float ((d ': ds) :: [Nat])
dfFloat x = realToFrac x

as3 :: p (ds :: [Nat]) -> ((Take 3 ds) :~: '[3,2,5], ds :~: '[3,2,5])
as3 _ = unsafeCoerce (Refl, Refl)

asCons :: p (ds :: [Nat]) -> ds :~: (a ': as)
asCons _ = unsafeCoerce Refl

asNil :: p ds -> ds :~: '[]
asNil _ = unsafeCoerce Refl

--   print (two + vec2 3 4)
--   print (two + vec2 3 4 + 5)
--   print (two <:> two == two <:> x)
--   print (two <:> two == two <:> two)
--   print (two <:> x)
--   print x
--   print (x < two)
--   print (fromBytes (toBytes x) + two / 7 + 5)
--   print ((x <:> two * 3 <:> two) / 4.2 <:> x)
--   putStrLn "Done pure!"
--   ptr <- mallocArray 3
--   poke ptr $ Store x
--   pokeElemOff ptr 1 5
--   pokeByteOff ptr 16 $ Store two
--   peek ptr >>= print
--   peekByteOff ptr 8 >>= (print :: Store Vec2f -> IO ())
--   peek (plusPtr ptr 8 :: Ptr (Store Vec2f)) >>= print
--   peekElemOff ptr 2 >>= print
--   peek (plusPtr ptr 16 :: Ptr (Store Vec2f)) >>= print
--   peekByteOff ptr 0 >>= (print :: Store (Vec Float 6) -> IO ())
--   peekByteOff ptr 4 >>= (print :: Store (Vec Float 3) -> IO ())
--   putStrLn "Done IO!"
--
--
--   putStrLn "Matrices..."
--   print $ index 1 2 m1
--   print m1
--   putStrLn (show (index 1 1 m1) ++ " " ++ show (index 1 2 m1) )
--   putStrLn (show (index 2 1 m1) ++ " " ++ show (index 2 2 m1) )
--   print (indexCol 1 m1)
--   print (indexCol 2 m1)
--   print (indexRow 1 m1)
--   print (indexRow 2 m1)
--   print (indexRow 3 m1)
-- --  print (M.indexMat 2 4 m1)
-- --  print (M.indexCol 4 m1 :: V.Vec2f)
--
--   putStrLn "Matrix products"
--   print y2
--
--   putStrLn "EasyTensor"
-- --  print a
-- --  print b
-- --  print c
-- --  print d
-- --  print (a %* c)
--   print (two <:> x <:> 7 / x)
--   print m32
--   print m33
-- --  print x3
-- --  print v3
-- --  print (v3 // 4)
-- --  print (10 \\ 4 :: Tensor Double 1 1)
-- --  print $ a %* c
-- --  print $ transpose x3 %* m32
--   putStrLn "m33"
--   print m33
--   putStrLn "determinants"
--   print (2 * eye :: Tensor Float 5 5)
--   print $ det (2 * eye :: Tensor Float 5 5)
--   print m33
--   print $ det m33
--   print $ two <:> x
--   print $ det (two <:> x)
--   print (x3 <:> (m33 %* x3) <:> (m33 %* x3))
--   print $ det (x3 <:> (m33 %* x3) <:> (m33 %* x3))
--   putStrLn "Inverse"
--   print m33
--   print (inverse m33)
--   print $ inverse m33 %* m33
--   print $ m33 %* inverse m33
--   print (2 * eye :: Tensor Float 5 5)
--   print $ inverse (2 * eye :: Tensor Float 5 5)
--   print $ (2 * eye :: Tensor Float 5 5) * inverse (2 * eye :: Tensor Float 5 5)
--   print (x3 <:> v3 <:> v3)
--   print $ inverse (x3 <:> v3 <:> v3)
--   print $ (x3 <:> v3 <:> v3) %* inverse (x3 <:> v3 <:> v3)
-- --  print (ewgen (\(i,_) -> realToFrac $ i) :: Tensor Float 5 10)
-- --  print (ewgen (\(_,j) -> realToFrac $ j) :: Tensor Float 4 2)
-- --  print (ewgen (\(i,j) -> realToFrac $ i*j) :: Tensor Float 1 12)
-- --  print (ewgen (\(i,j) -> realToFrac $ i*j) :: Tensor Float 2 1)
-- --  print $ ewmap (\(i,j) x' -> x' + 100 * realToFrac i + 1000 * realToFrac j) m33
-- --  print $ ((abs $ (ewgen (\(i,j) -> realToFrac $ i*j) :: Tensor Float 12 2)
-- --           %*
-- --           (ewgen (\(i,j) -> realToFrac j / realToFrac i - realToFrac i / realToFrac j) :: Tensor Float 2 20))
-- --          / fill 15) %* (ewgen (\(i,j) -> realToFrac $ i*j) :: Tensor Float 20 2) %* two
-- --  print $ head . drop 10000 $ iterate inverse (two <:> x)
--   print arr3D
--  print $ elementWise (\x -> Just x) (3 :: T.NDArray Float '[2,3,1])
--  print $ T.dropDims (Proxy :: Proxy 0) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 1) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 2) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 3) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 4) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 5) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 6) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.dropDims (Proxy :: Proxy 7) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 0) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 1) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 2) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 3) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 4) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 5) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 6) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  print $ T.takeDims (Proxy :: Proxy 7) $ (2 :- 3 :- 4 :- 7 :- 8 :- Z :: Dim '[4,4,4,10,10])
--  mapM_ print $ [minBound .. dim1]
--  putStrLn "Manupulations with ids"
--  print dim1
--  print $ T.stepDim 0 dim1
--  print $ T.stepDim 4 dim1
--  print $ T.stepDim (-4) dim1
--  print $ T.stepDim 16 dim1
--  print $ T.stepDim (-16) dim1
--  print $ T.stepDim 64 dim1
--  print $ T.stepDim (-64) dim1
--  print $ T.stepDim 640 dim1
--  print $ T.stepDim (-640) dim1
--  print $ T.stepDim 6400 dim1
--  print $ T.stepDim (-6400) dim1
--  putStrLn "Manupulations with ids -- succ"
--  print $ T.succDim dim1
--  print $ T.stepDim 1 dim1
--  print . T.succDim . T.succDim $ dim1
--  print $ T.stepDim 2 dim1
--  print . T.succDim . T.succDim . T.succDim $ dim1
--  print $ T.stepDim 3 dim1
--  print . T.succDim . T.succDim . T.succDim . T.succDim $ dim1
--  print $ T.stepDim 4 dim1
--  print . T.succDim . T.succDim . T.succDim . T.succDim . T.succDim $ dim1
--  print $ T.stepDim 5 dim1
--  print . T.succDim . T.succDim . T.succDim . T.succDim . T.succDim . T.succDim $ dim1
--  print $ T.stepDim 6 dim1
--  print $ T.predDim dim1
--  print $ T.stepDim (-1) dim1
--  print . T.predDim . T.predDim $ dim1
--  print $ T.stepDim (-2) dim1
--  print . T.predDim . T.predDim . T.predDim $ dim1
--  print $ T.stepDim (-3) dim1
--  print . T.predDim . T.predDim . T.predDim . T.predDim $ dim1
--  print $ T.stepDim (-4) dim1
--  print . T.predDim . T.predDim . T.predDim . T.predDim . T.predDim $ dim1
--  print $ T.stepDim (-5) dim1
--  print . T.predDim . T.predDim . T.predDim . T.predDim . T.predDim . T.predDim $ dim1
--  print $ T.stepDim (-6) dim1
--  putStrLn "Enum from.."
--  print $ [dim2..]
--  print $ [pred dim2..dim2u]
--  print $ [dim2u..dim2]
--  print $ [dim2u..dim2u]
--  print $ [dim2..dim2]
--  putStrLn "Dim diffs"
--  print $ T.diffDim dim2u dim2
--  print $ T.diffDim dim2 dim2u
--  print $ T.diffDim dim2 dim2
--  print $ T.diffDim dim2u dim2u
--  print $ T.diffDim dim2 maxBound
--  putStrLn "Enum from then to.."
--  print $ [dim2,dim2u..]
--  print $ [dim2u,dim2..]
--  print $ [dim2u,dim2u..]
--  print $ [dim2,dim2..]
--  print $ [dim2,dim2u..pred maxBound]
--  print $ [dim2,succ dim2u..pred maxBound]
--  print $ [dim2,succ dim2u..maxBound]
--  print $ [dim2u,dim2..minBound]
--  print $ [succ dim2u,dim2..succ minBound]
--  print $ [succ dim2u,dim2..minBound]
--  print $ [dim2u,dim2u..minBound]
--  print $ [dim2u,dim2u..maxBound]
--   where
--     two = vec2 2 2.001 :: Vec2f
--     x = two / vec2 3.2 (-2)
--     m1 = fromBytes (toBytes (two <:> x <:> 7 / x)) :: Mat Float 3 2
--     m32 = m1
--     x2 = 7 :: Vec2f
--     x3 = 9 :: Vec3f
--     y2 = m32 %* x2
--     m33 = m32 <:> 17
--     v3 = m33 %* x3
-- --    dim1 = 2 :- 3 :- 4 :- 7 :- 2 :- Z :: Dim '[4,4,4,10,3]
-- --    dim2u = 1 :- 3 :- 1 :- Z :: Dim '[2,3,2]
-- --    dim2 = 2 :- 2 :- 1 :- Z :: Dim '[2,3,2]
--     arr3D = ewmap (\i f -> f + realToFrac (fromEnum i) )
--             3 :: T.NDArray Float '[4,3,3]
