{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
module Numeric.PrimBytesTest (runTests) where

import Data.Int
import Data.Type.Lits
import Data.Word
import Foreign.Storable
import GHC.Exts
import GHC.Generics
import Numeric.PrimBytes
import Test.QuickCheck


data Vertex a b c
  = Vertex
  { pos         :: (a, a, a, a)
  , norm        :: (b, b, b)
  , tex         :: Either (b, b) (c, c, c)
  , extraFloats :: (Float, Double)
  } deriving (Generic, Show, Read, Eq, Ord)


data ManyAlternatives a b c
  = EmptyAlt
  | FirstAlt a b b c
  | SecondAlt a c
  | AltN (Either a b)
  | AltM (Maybe a) (Maybe b) (Maybe c)
  | SecondEmptyAlt
  | SomeWeirdChoice Word8 Word64 Int8 Char Word32 Int16 a
  deriving (Generic, Show, Read, Eq, Ord)

type Ver1 = Vertex Double Float Char
type Ver2 = Vertex Word8 Double Int16
type Ver3 = Vertex Word16 Word32 Word64
type Ver4 = Vertex Int64 Int8 Int32
type MA1 = ManyAlternatives Word8 Double Double
type MA2 = ManyAlternatives Float Double Int16

instance (PrimBytes a, PrimBytes b, PrimBytes c)
      => PrimBytes (Vertex a b c)
instance (PrimBytes a, PrimBytes b, PrimBytes c)
      => PrimBytes (ManyAlternatives a b c)
instance (PrimBytes a, PrimBytes b, PrimBytes c) => Storable (Vertex a b c) where
    sizeOf = bSizeOf
    alignment = bAlignOf
    peekElemOff = bPeekElemOff
    pokeElemOff = bPokeElemOff
    peekByteOff = bPeekByteOff
    pokeByteOff = bPokeByteOff
    peek = bPeek
    poke = bPoke

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Vertex a b c) where
    arbitrary = Vertex <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (ManyAlternatives a b c) where
    arbitrary = choose (1, 7 :: Int) >>= \case
      1 -> pure EmptyAlt
      2 -> FirstAlt <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      3 -> SecondAlt <$> arbitrary <*> arbitrary
      4 -> AltN <$> arbitrary
      5 -> AltM <$> arbitrary <*> arbitrary <*> arbitrary
      6 -> pure SecondEmptyAlt
      _ -> SomeWeirdChoice <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                         <*> arbitrary <*> arbitrary <*> arbitrary

-- The most basic property
fromToBytesId :: (PrimBytes a, Eq a, Show a) => a -> Property
fromToBytesId v = v === fromBytes (byteOffset v) (getBytes v)

-- Check whether @byteFieldOffset@ calculates correct field offsets
vertexFields :: ( PrimBytes a, Eq a, Show a
                , PrimBytes b, Eq b, Show b
                , PrimBytes c, Eq c, Show c)
             => Vertex a b c -> Property
vertexFields v
  | ba <- getBytes v
  , off <- byteOffset v
    = conjoin
    [ counterexample "pos" $ pos v === fromBytes
        (off +# byteFieldOffset (proxy# @Symbol @"pos") v) ba
    , counterexample "norm" $ norm v === fromBytes
       (off +# byteFieldOffset (proxy# @Symbol @"norm") v) ba
    , counterexample "tex" $ tex v === fromBytes
       (off +# byteFieldOffset (proxy# @Symbol @"tex") v) ba
    , counterexample "extraFloats" $ extraFloats v === fromBytes
       (off +# byteFieldOffset (proxy# @Symbol @"extraFloats") v) ba
    ]



prop_fromToBytesIdMaybe :: Maybe Int -> Property
prop_fromToBytesIdMaybe = fromToBytesId
prop_fromToBytesIdEither :: Either Int Double -> Property
prop_fromToBytesIdEither = fromToBytesId
prop_fromToBytesIdVer1 :: Ver1 -> Property
prop_fromToBytesIdVer1 = fromToBytesId
prop_fromToBytesIdVer2 :: Ver2 -> Property
prop_fromToBytesIdVer2 = fromToBytesId
prop_fromToBytesIdVer3 :: Ver3 -> Property
prop_fromToBytesIdVer3 = fromToBytesId
prop_fromToBytesIdVer4 :: Ver4 -> Property
prop_fromToBytesIdVer4 = fromToBytesId
prop_fromToBytesIdMA1 :: MA1 -> Property
prop_fromToBytesIdMA1 = fromToBytesId
prop_fromToBytesIdMA2 :: MA2 -> Property
prop_fromToBytesIdMA2 = fromToBytesId
prop_vertexFieldsVer1 :: Ver1 -> Property
prop_vertexFieldsVer1 = vertexFields
prop_vertexFieldsVer2 :: Ver2 -> Property
prop_vertexFieldsVer2 = vertexFields
prop_vertexFieldsVer3 :: Ver3 -> Property
prop_vertexFieldsVer3 = vertexFields
prop_vertexFieldsVer4 :: Ver4 -> Property
prop_vertexFieldsVer4 = vertexFields

return []
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
