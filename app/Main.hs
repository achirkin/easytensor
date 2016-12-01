
{-# LANGUAGE DataKinds #-}
module Main where


--import qualified Numeric.Tensor as T
import Numeric.Commons
-- import Numeric.Vector ((<:>))
-- import qualified Numeric.Vector as V
-- import qualified Numeric.Matrix as M
import Numeric.EasyTensor

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal

main :: IO ()
main = do
  putStrLn "Hello world!"
  print (two + vec2 3 4)
  print (two + vec2 3 4 + 5)
  print (two <:> two == two <:> x)
  print (two <:> two == two <:> two)
  print (two <:> x)
  print x
  print (x < two)
  print (fromBytes (toBytes x) + two / 7 + 5)
  print ((x <:> two * 3 <:> two) / 4.2 <:> x)
  putStrLn "Done pure!"
  ptr <- mallocArray 3
  poke ptr x
  pokeElemOff ptr 1 5
  pokeByteOff ptr 16 two
  peek ptr >>= print
  peekByteOff ptr 8 >>= (print :: Vec2f -> IO ())
  peek (plusPtr ptr 8 :: Ptr Vec2f) >>= print
  peekElemOff ptr 2 >>= print
  peek (plusPtr ptr 16 :: Ptr Vec2f) >>= print
  peekByteOff ptr 0 >>= (print :: Vec Float 6 -> IO ())
  peekByteOff ptr 4 >>= (print :: Vec Float 3 -> IO ())
  putStrLn "Done IO!"


  putStrLn "Matrices..."
  print m1
  -- putStrLn (show (M.indexMat 1 1 m1) ++ " " ++ show (M.indexMat 1 2 m1) ++ " " ++ show (M.indexMat 1 3 m1))
  -- putStrLn (show (M.indexMat 2 1 m1) ++ " " ++ show (M.indexMat 2 2 m1) ++ " " ++ show (M.indexMat 2 3 m1))
  -- print (M.indexCol 1 m1 :: Vec2f)
  -- print (M.indexCol 2 m1 :: Vec2f)
  -- print (M.indexCol 3 m1 :: Vec2f)
  -- print (M.indexRow 1 m1)
  -- print (M.indexRow 2 m1)
--  print (M.indexMat 2 4 m1)
--  print (M.indexCol 4 m1 :: V.Vec2f)

  putStrLn "Matrix products"
  print y2

  putStrLn "EasyTensor"
  print a
  print b
  print c
  print d
  print (a `prod` c)
  where
    two = vec2 2 2.001 :: Vec2f
    x = two / vec2 3.2 (-2)
    m1 = fromBytes (toBytes (two <:> x <:> 7 / x)) :: Mat Float 2 3
    m23 = m1
    x3 = 7 :: Vec3f
    y2 = m23 `prod` x3

    a = 1 :: Tensor Float 2 2
    b = 3 :: Tensor Float 1 1
    c = 4 :: Tensor Float 2 1
    d = 5 :: Tensor Float 1 2
