{-# LANGUAGE DataKinds #-}
module Main where


--import qualified Numeric.Tensor as T
import Numeric.Commons
import Numeric.Vector ((<:>))
import qualified Numeric.Vector as V

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal

main :: IO ()
main = do
  putStrLn "Hello world!"
  print (two + V.vec2 3 4)
  print (two + V.vec2 3 4 + 5)
  print x
  print (x < two)
  print (fromBytes (toBytes x) + two / 7 + 5)
  print (x <:> two)
  putStrLn "Done pure!"
  ptr <- mallocArray 3
  poke ptr x
  pokeElemOff ptr 1 5
  pokeByteOff ptr 16 two
  peek ptr >>= print
  peekByteOff ptr 8 >>= (print :: V.Vec2f -> IO ())
  peek (plusPtr ptr 8 :: Ptr V.Vec2f) >>= print
  peekElemOff ptr 2 >>= print
  peek (plusPtr ptr 16 :: Ptr V.Vec2f) >>= print
  peekByteOff ptr 0 >>= (print :: V.Vector Float 6 -> IO ())
  peekByteOff ptr 4 >>= (print :: V.Vector Float 3 -> IO ())
  putStrLn "Done IO!"
  where
    two = V.vec2 2 2.001 :: V.Vec2f
    x = two / V.vec2 3.2 (-2)
