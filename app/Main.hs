
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE IncoherentInstances #-}

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
-- import Data.Type.Equality
import Numeric.Dimensions
import Numeric.DataFrame
import Numeric.Commons
-- import Unsafe.Coerce
import Data.Functor.Identity
import Data.Functor.Const
import Data.Semigroup

main :: IO ()
main = do
    putStrLn "Hello world!"
    print (Proxy @3 :* Proxy @2 :* (D :: Dim ('[] :: [Nat])))
    print $ case (,,,) <$> someNatVal 3
                                    <*> someNatVal 6
                                    <*> someNatVal 8
                                    <*> someNatVal 4
                                    of
      Nothing -> Nothing
      Just (a,b,c,d) -> someDimVal $ a :? b :? c :? d :? D
    printEither s
    printEither s2
    -- look at this hole: amazing type inference!
    -- putStrLn $ _ x3
    printEither s3
    print x3
    print dfY
    print $ vec2 2 (3 :: Float) %* mat22 (vec2 1 0) (vec2 0 2)
    print $ vec2 2 3 %* matX
    print $ vec2 2 3 %* transpose matX
    print $ matX %* transpose matX
    print $ det matX
    print $ inverse matX
    print dfY2
    print $ runSlice (subSpace (\_ mat -> Const (Sum mat) :: Const (Sum (DataFrame Float '[3, 2])) Scf)) ixs
    print $ runSlice (slice (Get 1 :& 3) pleaseFire) ixs
    print $ runSlice (slice (Get 1 :& 3 :& 4) (const $ Identity . (* 0.7))) ixs
    print dimVec2
    print sVec2
    print $ index 2 (Identity . (+0.5)) ixs
    print $ index 3 Const ixs
    print ixs
    print $ index 3 Const ixs -- xx.yy.3
    print $  (index 3 . index 1) Const ixs -- xx.1.3
    print $  (index 3 . index 1 . index 2) Const ixs -- 2.1.3
    print $  (index 2 . index 2 . index 2) Const ixs -- 2.2.2
    print $ totalDim ixs
    print $ totalDim (Proxy @'[3,2])
    print $ totalDim (Proxy @'[3])
    print $ totalDim (Proxy @'[])
  where
    pleaseFire :: Idx i -> DataFrame Float '[3, 2] -> Const (Sum (DataFrame Float '[3, 2])) Scf
    pleaseFire _ = Const . Sum
    ixs = ewgen (\(i:!j:!k:!Z) -> realToFrac $ i*100 + j*10 + k) :: DFF '[3,2,4]
    matX = mat22 (vec2 0 2) (vec2 1 (0 :: Float))
    printEither :: Either String String -> IO ()
    printEither (Left a) = putStrLn a
    printEither (Right a) = putStrLn a
    Just d2 = someNatVal 2
    Just d3 = someNatVal 5
    dimX :: Dim '[N 3, XN, XN, N 2]
    dimX = Proxy :* d2 :? d3 :? Proxy :* D
    dimVec2 :: Dim '[XN]
    dimVec2 = d2 :? D
    sVec2 = withDim dimVec2 (\ds -> show (dfFloat 3.11 `inSpaceOf` ds)
                            )
    s2 = withDim dimX (\ds -> show (dfFloat (exp 3) `inSpaceOf` ds)
                      )
    x3 = case withDim dimX (\ds -> unboundShape $ dfFloat 42.0001 `inSpaceOf` ds
                      ) of
        Right x -> Right $ x `inSpaceOf` Proxy @'[XN,XN,XN,N _]
        Left a -> Left a
    s = withDim dimX (\ds -> let pix = 2 * dfFloat pi `inSpaceOf` ds
                             in show pix
                              ++ show (pix ! (1 :! 2 :! 1 :! 2) )
                     )
      :: Either String String
    s3 = (`withShape` show) <$> x3
    -- dimX1 :: Dim '[3,2,4]
    -- dimX1 = dim
    -- dimX2 :: Dim '[4,5,2]
    -- dimX2 = dim
    dfX1  :: DFF '[3,2,4]
    dfX1  = pi
    dfX2  :: DFF '[4,5]
    dfX2  = 1
    -- dfY :: DFF '[3,2,5]
    dfY   = dfX1 %* dfX2
    dfY2  = (runSlice . slice (Get 4 )
                      $ slice (Get 2 :& 1 :& 1)
                      (\(i :! j :! Z) v -> [ scalar (v ! 1) - realToFrac i
                                           , scalar (v ! 2) *2 / realToFrac j])) dfY

dfFloat :: Fractional (DataFrame Float ds)
        => Float -> DataFrame Float (ds :: [Nat])
dfFloat = realToFrac


type DFF (ds :: [Nat]) = DataFrame Float ds

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
