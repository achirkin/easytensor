{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDeriving     #-}
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

import           Data.Int
import           Data.Type.Lits
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Exts
import           GHC.Generics
import           GHC.Word
import           Numeric.Arbitraries  ()
import           Numeric.Dimensions
import           Numeric.PrimBytes
import qualified Numeric.Tuple.Lazy   as LT
import qualified Numeric.Tuple.Strict as ST
import           Spec.Util
import           Test.QuickCheck

data Dummy
  deriving (Generic, Show, Read, Eq, Ord)

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

data MyUnboxedType a
  = VeryUnboxed Word# Char# Int# Addr# Float# Double#
  | NotSoUnboxed a a
  | JustABitUnboxed Double# a Char# a
  deriving (Generic, Eq, Ord)

instance Show a => Show (MyUnboxedType a) where
  show (VeryUnboxed w c i a f d)
    = unwords [ "VeryUnboxed", show (W# w), show (I# i)
              , show (C# c), show (Ptr a), show (F# f), show (D# d)]
  show (NotSoUnboxed a b)
    = unwords [ "NotSoUnboxed", show a, show b]
  show (JustABitUnboxed d x c y)
    = unwords [ "JustABitUnboxed ", show (D# d), show x, show (C# c), show y]


instance (PrimBytes a, PrimBytes b, PrimBytes c)
      => PrimBytes (Vertex a b c)
instance (PrimBytes a, PrimBytes b, PrimBytes c)
      => PrimBytes (ManyAlternatives a b c)
instance (PrimBytes a)
      => PrimBytes (MyUnboxedType a)
instance (PrimBytes a, PrimBytes b, PrimBytes c) => Storable (Vertex a b c) where
    sizeOf = bSizeOf
    alignment = bAlignOf
    peekElemOff = bPeekElemOff
    pokeElemOff = bPokeElemOff
    peekByteOff = bPeekByteOff
    pokeByteOff = bPokeByteOff
    peek = bPeek
    poke = bPoke
instance (PrimBytes a, PrimBytes b, PrimBytes c) => Storable (ManyAlternatives a b c) where
    sizeOf = bSizeOf
    alignment = bAlignOf
    peekElemOff = bPeekElemOff
    pokeElemOff = bPokeElemOff
    peekByteOff = bPeekByteOff
    pokeByteOff = bPokeByteOff
    peek = bPeek
    poke = bPoke
instance PrimBytes a => Storable (MyUnboxedType a) where
    sizeOf = bSizeOf
    alignment = bAlignOf
    peekElemOff = bPeekElemOff
    pokeElemOff = bPokeElemOff
    peekByteOff = bPeekByteOff
    pokeByteOff = bPokeByteOff
    peek = bPeek
    poke = bPoke

instance Arbitrary CBool where
    arbitrary = CBool <$> arbitrary

instance Arbitrary (Ptr Dummy) where
    arbitrary = intPtrToPtr . IntPtr <$> arbitrary

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

instance Arbitrary a => Arbitrary (MyUnboxedType a) where
    arbitrary = choose (1, 3 :: Int) >>= \case
      1 -> do
        W# w <- arbitrary
        C# c <- arbitrary
        I# i <- arbitrary
        Ptr a <- arbitrary :: Gen (Ptr Dummy)
        F# f <- arbitrary
        D# d <- arbitrary
        return $ VeryUnboxed w c i a f d
      2 -> NotSoUnboxed <$> arbitrary <*> arbitrary
      _ -> do
        C# c <- arbitrary
        D# d <- arbitrary
        x <- arbitrary
        y <- arbitrary
        return $ JustABitUnboxed d x c y

showByteArray :: Int# -> ByteArray# -> [Word8]
showByteArray size ba = go 0#
  where
    go i
      | isTrue# (i <# size) = W8# (indexWord8Array# ba i) : go (i +# 1#)
      | otherwise = []

showMutableByteArray :: Int# -> MutableByteArray# s
                     -> State# s -> (# State# s, [Word8] #)
showMutableByteArray size mba = go 0#
  where
    go i s0
      | isTrue# (i <# size)
      , (# s1, w  #) <- readWord8Array# mba i s0
      , (# s2, ws #) <- go (i +# 1#) s1
                  = (# s2, W8# w : ws #)
      | otherwise = (# s0, [] #)

-- | The most basic property
fromToBytesId :: (PrimBytes a, Eq a, Show a) => a -> Property
fromToBytesId v = counterexample msg $
    v === fromBytes (byteOffset v) bytes
  where
    bytes = getBytes v
    byteList  = showByteArray (byteOffset v +# byteSize v) bytes
    msg = unlines
      [ "byteOffset:    " ++ show (I# (byteOffset v))
      , "byteAlign:     " ++ show (I# (byteAlign v))
      , "byteSize:      " ++ show (I# (byteSize v))
      , "Array content: " ++ show byteList
      ]

-- | Pointers
fromToPtrId :: (PrimBytes a, Eq a, Show a) => a -> Property
fromToPtrId v = ioProperty . allocaBytes (bSizeOf v) $ \ptr -> do
  bPoke ptr v
  v1 <- bPeek ptr
  return (v === v1)

-- | Byte offsets
readWriteBytesId :: (PrimBytes a, Eq a, Show a) => Int -> a -> Property
readWriteBytesId off' v = case runRW# go of (# _, r #) -> r
  where
    -- reasonably small offset aligned to a multiple of @byteAlign v@
    off = case abs off' `mod` 100 of I# i -> i *# byteAlign v
    -- write and read the value from an array by a specified offset
    go :: forall s . State# s -> (# State# s, Property #)
    go s0
      | (# s1, mba #) <- newByteArray# (byteSize v +# off) s0
      , s2            <- writeBytes mba off v s1
      , (# s3, w #)   <- readBytes mba off s2
      , (# s4, byteList #) <- showMutableByteArray (byteSize v +# off) mba s3
      , (# s5, ba #)  <- unsafeFreezeByteArray# mba s4
      , u <- fromBytes off ba
      , msg <- unlines
              [ "Offset 'off':  " ++ show (I# off)
              , "byteOffset:    " ++ show (I# (byteOffset v))
              , "byteAlign:     " ++ show (I# (byteAlign v))
              , "byteSize:      " ++ show (I# (byteSize v))
              , "Array content: " ++ show byteList
              ]
         = (# s5, counterexample msg (v === w .&&. v == u) #)

-- | Elem offsets
readWriteArrayId :: (PrimBytes a, Eq a, Show a) => Int -> Int -> a -> Property
readWriteArrayId n' i' v = case runRW# go of (# _, r #) -> r
  where
    -- allocate reasonably small number of elements
    n = case abs n' `mod` 30 of I# x -> x +# 1#
    i = case abs i' `mod` I# n of I# x -> x
    -- write and read the value from an array by a specified offset
    go :: forall s . State# s -> (# State# s, Property #)
    go s0
      | (# s1, mba #) <- newByteArray# (n *# byteSize v) s0
      , s2            <- writeArray mba i v s1
      , (# s3, w #)   <- readArray mba i s2
      , (# s4, byteList #) <- showMutableByteArray (n *# byteSize v) mba s3
      , (# s5, ba #)  <- unsafeFreezeByteArray# mba s4
      , u <- indexArray ba i
      , msg <- unlines
              [ "Array size:    " ++ show (I# n)
              , "Array index:   " ++ show (I# i)
              , "byteOffset:    " ++ show (I# (byteOffset v))
              , "byteAlign:     " ++ show (I# (byteAlign v))
              , "byteSize:      " ++ show (I# (byteSize v))
              , "Array content: " ++ show byteList
              ]
         = (# s5, counterexample msg (v === w .&&. v == u) #)


-- | Working this elem offsets and byte offsets together
mixedTransformsId :: (PrimBytes a, Eq a, Show a) => Int -> Int -> a -> Property
mixedTransformsId n' i' v = case runRW# go of (# _, r #) -> r
  where
    -- allocate reasonably small number of elements
    n = case abs n' `mod` 30 of I# x -> x +# 2#
    i = case abs i' `mod` (I# n - 1) of I# x -> x -- i+1 is always a valid index
    i1 = i +# 1#
    -- write and read the value from an array by a specified offset
    go :: forall s . State# s -> (# State# s, Property #)
    go s0
      | (# s1, mba #) <- newByteArray# (n *# byteSize v) s0
      , s2            <- writeArray mba i1 v s1
      , s3            <- writeBytes mba (i *# byteSize v) v s2
      , (# s4, w #)   <- readArray mba i s3
      , (# s5, u #)   <- readBytes mba (i1 *# byteSize v) s4
      , (# s6, byteList #) <- showMutableByteArray (n *# byteSize v) mba s5
      , msg <- unlines
              [ "Array size:    " ++ show (I# n)
              , "Array index:   " ++ show (I# i)
              , "byteOffset:    " ++ show (I# (byteOffset v))
              , "byteAlign:     " ++ show (I# (byteAlign v))
              , "byteSize:      " ++ show (I# (byteSize v))
              , "Array content: " ++ show byteList
              ]
         = (# s6, counterexample msg (v === w .&&. v == u) #)


class SamePrimRep a b where
  convert :: a -> b

-- | Some times should have the same PrimBytes representation
samePrimRepId :: forall a b
               . ( PrimBytes a, PrimBytes b, Eq b, Show b, SamePrimRep a b)
              => Int -> a -> b -> Property
samePrimRepId off' a b = case runRW# go of (# _, r #) -> r
  where
    f = convert @a @b -- monomorphise it to avoid type errors
    -- reasonably small offset aligned to a multiple of @byteAlign a@
    off = case abs off' `mod` 100 of I# i -> i *# byteAlign a
    -- write and read the value from an array by a specified offset
    go :: forall s . State# s -> (# State# s, Property #)
    go s0
      | (# s1, mba #) <- newByteArray# (off +# byteSize a +# byteSize b) s0
      , s2            <- writeBytes mba off a s1
      , s3            <- writeBytes mba (off +# byteSize a) b s2
      , (# s4, a' #)  <- readBytes mba off s3
      , (# s5, b' #)  <- readBytes mba (off +# byteSize b) s4
      , (# s6, bl #)  <- showMutableByteArray (off +# byteSize a +# byteSize b) mba s5
      , msg <- unlines
              [ "Offset 'off':  " ++ show (I# off)
              , "byteOffset:    " ++ show (I# (byteOffset a), I# (byteOffset b))
              , "byteAlign:     " ++ show (I# (byteAlign a), I# (byteAlign b))
              , "byteSize:      " ++ show (I# (byteSize a), I# (byteSize b))
              , "Array content: " ++ show bl
              ]
         = (# s6, counterexample msg (f a === a' .&&. b === f b') #)

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



-- a list of types to test the properties against
type SingleVarTypes =
   '[ Word8, Word16, Word32, Word64, Word
    , Int8, Int16, Int32, Int64, Int
    , Char, Double, Float
    , CChar, CSChar, CUChar, CShort, CUShort
    , CInt, CUInt, CLong, CULong
    , CPtrdiff, CSize, CWchar, CSigAtomic
    , CLLong, CULLong, CBool, CIntPtr, CUIntPtr
    , CIntMax, CUIntMax, CClock, CTime
    , CUSeconds, CSUSeconds, CFloat, CDouble
    , Vertex Double Float Char
    , Vertex Word8 Double Int16
    , Vertex Word16 Word32 Word64
    , Vertex Int64 Int8 Int32
    , ManyAlternatives Word8 Double Double
    , ManyAlternatives Float Double Int16
    , Maybe Int
    , Either Int32 Double
    , MyUnboxedType (Either (Either () Char) Float)
    , MyUnboxedType ()
    , Ptr Dummy
    , Idx 7
    , Idxs ('[] :: [Nat])
    , Idxs '[5, 3, 12]
    , ST.Tuple '[Double, Word8, Word8, Word8, Maybe Float]
    , LT.Tuple '[]
    , LT.Tuple '[Int, Int8, Either Float Double]
    ]

-- I can substitute these in place of parameters of Vertex or ManyAlternatives
type TypeTriples =
  '[ '(Double, Float, Char)
   , '(Word8, Double, Int16)
   , '(Word16, Word32, Word64)
   , '(Word16, Word32, Word64)
   , '(Int64, Int8, Int32)
   , '(Float, Char, Char)
   , '(CChar, CInt, CPtrdiff)
   , '(CSChar, CUInt, CSize)
   , '(CUChar, CLong, CWchar)
   , '(CShort, CULong, CSigAtomic)
   , '(CUShort, CLLong, CIntMax)
   , '(CULLong, CUIntMax, CUSeconds)
   , '(CBool, CClock, CSUSeconds)
   , '(CIntPtr, CTime, CFloat)
   , '(CUIntPtr, CDouble, CDouble)
   ]

instance SamePrimRep a a where
  convert = id

instance SamePrimRep a a' => SamePrimRep a (ST.Tuple '[a']) where
  convert a = convert a ST.:$ U
instance (SamePrimRep a a', SamePrimRep b b') => SamePrimRep (a,b) (ST.Tuple '[a',b']) where
  convert (a, b) = convert a ST.:$ convert b ST.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c') => SamePrimRep (a,b,c) (ST.Tuple '[a',b',c']) where
  convert (a, b, c) = convert a ST.:$ convert b ST.:$ convert c ST.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c', SamePrimRep d d') => SamePrimRep (a,b,c,d) (ST.Tuple '[a',b',c',d']) where
  convert (a, b, c, d) = convert a ST.:$ convert b ST.:$ convert c ST.:$ convert d ST.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c', SamePrimRep d d', SamePrimRep e e') => SamePrimRep (a,b,c,d,e) (ST.Tuple '[a',b',c',d',e']) where
  convert (a, b, c, d, e) = convert a ST.:$ convert b ST.:$ convert c ST.:$ convert d ST.:$ convert e ST.:$ U

instance SamePrimRep a a' => SamePrimRep a (LT.Tuple '[a']) where
  convert a = convert a LT.:$ U
instance (SamePrimRep a a', SamePrimRep b b') => SamePrimRep (a,b) (LT.Tuple '[a',b']) where
  convert (a, b) = convert a LT.:$ convert b LT.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c') => SamePrimRep (a,b,c) (LT.Tuple '[a',b',c']) where
  convert (a, b, c) = convert a LT.:$ convert b LT.:$ convert c LT.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c', SamePrimRep d d') => SamePrimRep (a,b,c,d) (LT.Tuple '[a',b',c',d']) where
  convert (a, b, c, d) = convert a LT.:$ convert b LT.:$ convert c LT.:$ convert d LT.:$ U
instance (SamePrimRep a a', SamePrimRep b b', SamePrimRep c c', SamePrimRep d d', SamePrimRep e e') => SamePrimRep (a,b,c,d,e) (LT.Tuple '[a',b',c',d',e']) where
  convert (a, b, c, d, e) = convert a LT.:$ convert b LT.:$ convert c LT.:$ convert d LT.:$ convert e LT.:$ U

instance SamePrimRep (Maybe a) (Either () a) where
  convert (Just a) = Right a
  convert Nothing  = Left ()

instance SamePrimRep CChar Int8 where convert (CChar v) = v
instance SamePrimRep CSChar Int8 where convert (CSChar v) = v
instance SamePrimRep CSChar CChar where convert (CSChar v) = CChar v
instance SamePrimRep CUChar Word8 where convert (CUChar v) = v
instance SamePrimRep CShort Int16 where convert (CShort v) = v
instance SamePrimRep CUShort Word16 where convert (CUShort v) = v
instance SamePrimRep CInt Int32 where convert (CInt v) = v
instance SamePrimRep CUInt Word32 where convert (CUInt v) = v
instance SamePrimRep CLong Int64 where convert (CLong v) = v
instance SamePrimRep CULong Word64 where convert (CULong v) = v
instance SamePrimRep CPtrdiff Int64 where convert (CPtrdiff v) = v
instance SamePrimRep CSize Word64 where convert (CSize v) = v
instance SamePrimRep CWchar Int32 where convert (CWchar v) = v
instance SamePrimRep CSigAtomic Int32 where convert (CSigAtomic v) = v
instance SamePrimRep CLLong Int64 where convert (CLLong v) = v
instance SamePrimRep CULLong Word64 where convert (CULLong v) = v
instance SamePrimRep CBool Word8 where convert (CBool v) = v
instance SamePrimRep CIntPtr Int64 where convert (CIntPtr v) = v
instance SamePrimRep CUIntPtr Word64 where convert (CUIntPtr v) = v
instance SamePrimRep CIntMax Int64 where convert (CIntMax v) = v
instance SamePrimRep CUIntMax Word64 where convert (CUIntMax v) = v
instance SamePrimRep CClock Int64 where convert (CClock v) = v
instance SamePrimRep CTime Int64 where convert (CTime v) = v
instance SamePrimRep CUSeconds Word32 where convert (CUSeconds v) = v
instance SamePrimRep CSUSeconds Int64 where convert (CSUSeconds v) = v
instance SamePrimRep CFloat Float where convert (CFloat v) = v
instance SamePrimRep CDouble Double where convert (CDouble v) = v

type SamePrimRepTypes =
  '[ '( (Word, Double), ST.Tuple '[Word, Double] )
   , '( (Double, Word8, Word8, Word8, Maybe Float)
      , ST.Tuple '[Double, Word8, Word8, Word8, Maybe Float] )
   , '( (Word8, Int16, Word32, Either (Either Double Float) Int)
      , LT.Tuple '[Word8, Int16, Word32, Either (Either Double Float) Int] )
   , '( Maybe Float, Either () Float)
   , '( Maybe Double, Either () Double)
   , '( (CChar, CSChar, CSChar, CUChar), ST.Tuple '[Int8, Int8, CChar, Word8] )
   , '( (CShort, CUShort, CInt, CUInt), LT.Tuple '[Int16, Word16, Int32, Word32] )
   , '( (CLong, CULong, CPtrdiff, CSize), ST.Tuple '[Int64, Word64, Int64, Word64] )
   , '( (CWchar, CSigAtomic, CLLong, CULLong), LT.Tuple '[Int32, Int32, Int64, Word64] )
   , '( (CBool, CIntPtr, CUIntPtr, CIntMax, CUIntMax), ST.Tuple '[Word8, Int64, Word64, Int64, Word64] )
   , '( (CClock, CTime, CUSeconds, CSUSeconds), LT.Tuple '[Int64, Int64, Word32, Int64] )
   , '( (CFloat, CDouble), ST.Tuple '[Float, Double] )
   ]

return [] -- this is for $testWithTypes

prop_fromToBytesId :: Property
prop_fromToBytesId = $(testWithTypes 'fromToBytesId ''SingleVarTypes)

prop_fromToPtrId :: Property
prop_fromToPtrId = $(testWithTypes 'fromToPtrId ''SingleVarTypes)

prop_readWriteBytesId :: Property
prop_readWriteBytesId = $(testWithTypes 'readWriteBytesId ''SingleVarTypes)

prop_readWriteArrayId :: Property
prop_readWriteArrayId = $(testWithTypes 'readWriteArrayId ''SingleVarTypes)

prop_mixedTransformsId :: Property
prop_mixedTransformsId = $(testWithTypes 'mixedTransformsId ''SingleVarTypes)

prop_samePrimRepId :: Property
prop_samePrimRepId = $(testWithTypes 'samePrimRepId ''SamePrimRepTypes)

prop_vertexFields :: Property
prop_vertexFields = $(testWithTypes 'vertexFields ''TypeTriples)


return [] -- this is for $forAllProperties
runTests :: Int -> IO Bool
runTests n = $forAllProperties
  $ quickCheckWithResult stdArgs { maxSuccess = n }
