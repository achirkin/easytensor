{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Numeric.PrimBytes
  ( PrimBytes
    ( getBytes, fromBytes, readBytes, writeBytes, byteSize, byteAlign, byteOffset
    , indexArray, readArray, writeArray, readAddr, writeAddr)
  , PrimTag (..), primTag
  ) where

#include "MachDeps.h"

import           Data.Proxy              (Proxy (..))
import           GHC.Exts
import           GHC.Generics
import           GHC.Int
import           GHC.Word
import           Numeric.Dimensions.Idxs
import qualified Numeric.Tuple.Lazy      as TL
import qualified Numeric.Tuple.Strict    as TS
import qualified Numeric.Type.List       as L

-- | Facilities to convert to and from raw byte array.
class PrimTagged a => PrimBytes a where
    -- | Store content of a data type in a primitive byte array
    --   Should be used together with @byteOffset@ function.
    getBytes :: a -> ByteArray#
    -- | Load content of a data type from a primitive byte array
    fromBytes :: Int# -- ^ offset in bytes
              -> ByteArray#
              -> a
    -- | Read data from a mutable byte array given an offset in bytes
    readBytes :: MutableByteArray# s -- ^ source array
              -> Int# -- ^ byte offset of the source array
              -> State# s -> (# State# s, a #)
    -- | Write data into a mutable byte array at a given position (offset in bytes)
    writeBytes :: MutableByteArray# s -- ^ destination array
               -> Int# -- ^ byte offset of the destination array
               -> a -- ^ data to write into array
               -> State# s -> State# s
    -- | Read data from a specified address
    readAddr :: Addr# -> State# s -> (# State# s, a #)
    -- | Write data to a specified address
    writeAddr :: a -> Addr# -> State# s -> State# s
    -- | Size of a data type in bytes
    byteSize :: a -> Int#
    -- | Alignment of a data type in bytes.
    --   @byteOffset@ should be multiple of this value.
    byteAlign :: a -> Int#
    -- | Offset of the data in a byte array used to store the data,
    --   measured in bytes.
    --   Should be used together with @getBytes@ function.
    --   Unless in case of special data types represented by ByteArrays,
    --   it is equal to zero.
    byteOffset :: a -> Int#

    -- | Index array given an element offset.
    --
    --   > indexArray arr i = fromBytes ( i *# byteSize t) arr
    indexArray :: ByteArray# -> Int# -> a
    indexArray ba i = fromBytes (i *# byteSize @a undefined) ba
    {-# INLINE indexArray #-}

    -- | Read a mutable array given an element offset.
    --
    --   > readArray arr i = readBytes arr ( i *# byteSize t)
    readArray  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    readArray ba i = readBytes ba (i *# byteSize @a undefined)
    {-# INLINE readArray #-}

    -- | Write a mutable array given an element offset.
    --
    --   > writeArray arr i = writeBytes arr ( i *# byteSize t)
    writeArray :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
    writeArray ba i = writeBytes ba (i *# byteSize @a undefined)
    {-# INLINE writeArray #-}



    default getBytes :: (Generic a, GPrimBytes (Rep a)) => a -> ByteArray#
    getBytes a = ggetBytes (from a)
    {-# INLINE getBytes #-}

    default fromBytes :: (Generic a, GPrimBytes (Rep a))
                      => Int# -> ByteArray# -> a
    fromBytes i arr = to (gfromBytes 0## i arr)
    {-# INLINE fromBytes #-}

    default readBytes :: (Generic a, GPrimBytes (Rep a))
                      => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    readBytes mba i s = case greadBytes 0## mba i s of
      (# s', x #) -> (# s', to x #)
    {-# INLINE readBytes #-}

    default writeBytes :: (Generic a, GPrimBytes (Rep a))
                       => MutableByteArray# s -> Int# -> a -> State# s -> State# s
    writeBytes mba i = gwriteBytes 0## mba i . from
    {-# INLINE writeBytes #-}

    default readAddr :: (Generic a, GPrimBytes (Rep a))
                      => Addr# -> State# s -> (# State# s, a #)
    readAddr a s = case greadAddr 0## a s of
      (# s', x #) -> (# s', to x #)
    {-# INLINE readAddr #-}

    default writeAddr :: (Generic a, GPrimBytes (Rep a))
                       => a -> Addr# -> State# s -> State# s
    writeAddr = gwriteAddr 0## . from
    {-# INLINE writeAddr #-}


    default byteSize :: (Generic a, GPrimBytes (Rep a))
                     => a -> Int#
    byteSize a = gbyteSize (from a)
    {-# INLINE byteSize #-}

    default byteAlign :: (Generic a, GPrimBytes (Rep a))
                     => a -> Int#
    byteAlign a = gbyteAlign (from a)
    {-# INLINE byteAlign #-}

    default byteOffset :: (Generic a, GPrimBytes (Rep a))
                     => a -> Int#
    byteOffset a = gbyteOffset (from a)
    {-# INLINE byteOffset #-}


-- | Deriving `PrimBytes` using generics
class GPrimBytes f where
    ggetBytes :: f p -> ByteArray#
    gfromBytes :: Word# -- ^ Starting value of a constructor tag
               -> Int# -> ByteArray# -> f p
    greadBytes :: Word# -- ^ Starting value of a constructor tag
               -> MutableByteArray# s -> Int#  -> State# s -> (# State# s, f p #)
    gwriteBytes :: Word# -- ^ Starting value of a constructor tag
                -> MutableByteArray# s -> Int# -> f p -> State# s -> State# s
    greadAddr :: Word# -- ^ Starting value of a constructor tag
              -> Addr# -> State# s -> (# State# s, f p #)
    gwriteAddr :: Word# -- ^ Starting value of a constructor tag
               -> f p -> Addr# -> State# s -> State# s
    gbyteSize :: f p -> Int#
    gbyteAlign :: f p -> Int#
    gbyteOffset :: f p -> Int#
    -- | Number of constructors in the tree of a sum type.
    --   This is equal to one for all other types.
    gconTags    :: f p -> Word#


instance GPrimBytes V1 where
    -- Probably, this is illegal due to zero size of the array.
    -- There is no bottom to put here, but one should not call this anyway.
    ggetBytes _ = case runRW#
       ( \s0 -> case newByteArray# 0# s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr s1
       ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ _ _ = undefined
    {-# INLINE gfromBytes #-}
    greadBytes _ _ _ s = (# s, undefined #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ _ _ s = s
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ s = (# s, undefined #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ _ s = s
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = 0#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = 1#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 1##
    {-# INLINE gconTags #-}

instance GPrimBytes U1 where
    -- Probably, this is illegal due to zero size of the array.
    -- There is no bottom to put here, but one should not call this anyway.
    ggetBytes _ = case runRW#
       ( \s0 -> case newByteArray# 0# s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr s1
       ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ _ _ = U1
    {-# INLINE gfromBytes #-}
    greadBytes _ _ _ s = (# s, U1 #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ _ _ _ s = s
    {-# INLINE gwriteBytes #-}
    greadAddr _ _ s = (# s, U1 #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ _ _ s = s
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = 0#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = 1#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 1##
    {-# INLINE gconTags #-}

instance PrimBytes a => GPrimBytes (K1 i a) where
    ggetBytes ~(K1 a) = getBytes a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ i ba = K1 (fromBytes i ba)
    {-# INLINE gfromBytes #-}
    greadBytes _ = unsafeCoerce# (readBytes @a)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba i ~(K1 a) = writeBytes mba i a
    {-# INLINE gwriteBytes #-}
    greadAddr _ = unsafeCoerce# (readAddr @a)
    {-# INLINE greadAddr #-}
    gwriteAddr _ ~(K1 a) = writeAddr a
    {-# INLINE gwriteAddr #-}
    gbyteSize ~(K1 a) = byteSize a
    {-# INLINE gbyteSize #-}
    gbyteAlign ~(K1 a) = byteAlign a
    {-# INLINE gbyteAlign #-}
    gbyteOffset ~(K1 a) = byteOffset a
    {-# INLINE gbyteOffset #-}
    gconTags _ = 1##
    {-# INLINE gconTags #-}

instance GPrimBytes f => GPrimBytes (M1 i c f) where
    ggetBytes ~(M1 a) = ggetBytes a
    {-# NOINLINE ggetBytes #-}
    gfromBytes t i ba = M1 (gfromBytes t i ba)
    {-# INLINE gfromBytes #-}
    greadBytes = unsafeCoerce# (greadBytes @f)
    {-# INLINE greadBytes #-}
    gwriteBytes t  mba i ~(M1 a) = gwriteBytes t  mba i a
    {-# INLINE gwriteBytes #-}
    greadAddr = unsafeCoerce# (greadAddr @f)
    {-# INLINE greadAddr #-}
    gwriteAddr t ~(M1 a) = gwriteAddr t a
    {-# INLINE gwriteAddr #-}
    gbyteSize ~(M1 a) = gbyteSize a
    {-# INLINE gbyteSize #-}
    gbyteAlign ~(M1 a) = gbyteAlign a
    {-# INLINE gbyteAlign #-}
    gbyteOffset ~(M1 a) = gbyteOffset a
    {-# INLINE gbyteOffset #-}
    gconTags ~(M1 a) = gconTags a
    {-# INLINE gconTags #-}


instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :*: g) where
    -- | This function return not pinned byte array, which is aligned to
    --   @SIZEOF_HSWORD@.
    --   Thus, it ignores alignment of the underlying data type if it is larger.
    --   However, alignment calculation still makes sense for data types
    --   that are smaller than @SIZEOF_HSWORD@ bytes: they are packed more densely.
    ggetBytes xy = case runRW#
       ( \s0 -> case newByteArray# (gbyteSize xy) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (gwriteBytes 0## marr 0# xy s1)
       ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ i ba = x :*: y
      where
        x = gfromBytes 0## i ba
        y = gfromBytes 0## (i +# roundUpInt# (gbyteSize x) (gbyteAlign y)) ba
    {-# INLINE gfromBytes #-}
    greadBytes _ mba i s0 = case greadBytes 0## mba i s0 of
      (# s1, x #) -> case greadBytes 0## mba
                            (i +# roundUpInt# (gbyteSize x)
                                              (gbyteAlign @g undefined)
                            ) s1 of
        (# s2, y #) -> (# s2, x :*: y #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off ~(x :*: y) s =
      gwriteBytes 0## mba (off +# roundUpInt# (gbyteSize x) (gbyteAlign y)) y
      (gwriteBytes 0## mba off x s)
    {-# INLINE gwriteBytes #-}
    greadAddr _ addr s0 = case greadAddr 0## addr s0 of
      (# s1, x #) -> case greadAddr 0##
                            (plusAddr# addr
                              (roundUpInt# (gbyteSize x)
                                           (gbyteAlign @g undefined))
                            ) s1 of
        (# s2, y #) -> (# s2, x :*: y #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ ~(x :*: y) addr s =
      gwriteAddr 0## y (plusAddr# addr (roundUpInt# (gbyteSize x) (gbyteAlign y)))
      (gwriteAddr 0## x addr s)
    {-# INLINE gwriteAddr #-}
    gbyteSize ~(x :*: y)
      = gbyteSize y +# roundUpInt# (gbyteSize x) (gbyteAlign y)
    {-# INLINE gbyteSize #-}
    gbyteAlign ~(x :*: y) = maxInt# (gbyteAlign x) (gbyteAlign y)
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 1##
    {-# INLINE gconTags #-}


-- | Reserve 4 bytes for tag and try to pack alternatives as good as possible.
instance (GPrimBytes f, GPrimBytes g) => GPrimBytes (f :+: g) where
    ggetBytes xy = case runRW#
       ( \s0 -> case newByteArray# (gbyteSize xy) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (gwriteBytes 0## marr 0# xy s1)
       ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes toff off ba
      = case (# gconTags (undefined :: f a)
              , gconTags (undefined :: g a)
              , indexWord32Array# ba (uncheckedIShiftRL# off 2#)
                       `minusWord#` toff
              #) of
         (# 1##, _  , 0## #) -> L1 (gfromBytes 0## (off +# 4#) ba)
         (# cl , 1##, t   #)
           | isTrue# (eqWord# cl t) -> R1 (gfromBytes 0## (off +# 4#) ba)
         (# cl , _  , t   #)
           | isTrue# (geWord# cl t) -> L1 (gfromBytes toff off ba)
           | otherwise -> R1 (gfromBytes (plusWord# toff cl) off ba)
    {-# INLINE gfromBytes #-}
    greadBytes toff mba off s0
      = case readWord32Array# mba (uncheckedIShiftRL# off 2#) s0 of
        (# s1, tval #) -> case (# gconTags (undefined :: f a)
                                , gconTags (undefined :: g a)
                                , tval `minusWord#` toff
                                #) of
         (# 1##, _  , 0## #) -> case greadBytes 0## mba (off +# 4#) s1 of
             (# s2, r #) -> (# s2, L1 r #)
         (# cl , 1##, t   #)
           | isTrue# (eqWord# cl t) -> case greadBytes 0## mba (off +# 4#) s1 of
             (# s2, r #) -> (# s2, R1 r #)
         (# cl , _  , t   #)
           | isTrue# (geWord# cl t) -> case greadBytes toff mba off s1 of
             (# s2, r #) -> (# s2, L1 r #)
           | otherwise -> case greadBytes (plusWord# toff cl) mba off s1 of
             (# s2, r #) -> (# s2, R1 r #)
    {-# INLINE greadBytes #-}
    gwriteBytes t mba off (L1 x) s
      = case gconTags x of
          1## -> gwriteBytes 0## mba (off +# 4#) x
                 (writeWord32Array# mba (uncheckedIShiftRL# off 2#) t s)
          _   -> gwriteBytes t mba off x s
    gwriteBytes t mba off xy@(R1 y) s
      = case (# gconTags y, plusWord# t (gconTags (undef1 @f xy)) #) of
          (# 1## , t' #) -> gwriteBytes 0## mba (off +# 4#) y
              (writeWord32Array# mba (uncheckedIShiftRL# off 2#) t' s)
          (# _   , t' #) -> gwriteBytes t' mba off y s
    {-# INLINE gwriteBytes #-}
    greadAddr toff addr s0
      = case readWord32OffAddr# addr 0# s0 of
        (# s1, tval #) -> case (# gconTags (undefined :: f a)
                                , gconTags (undefined :: g a)
                                , tval `minusWord#` toff
                                #) of
         (# 1##, _  , 0## #) -> case greadAddr 0## (plusAddr# addr 4#) s1 of
             (# s2, r #) -> (# s2, L1 r #)
         (# cl , 1##, t   #)
           | isTrue# (eqWord# cl t) -> case greadAddr 0## (plusAddr# addr 4#) s1 of
             (# s2, r #) -> (# s2, R1 r #)
         (# cl , _  , t   #)
           | isTrue# (geWord# cl t) -> case greadAddr toff addr s1 of
             (# s2, r #) -> (# s2, L1 r #)
           | otherwise -> case greadAddr (plusWord# toff cl) addr s1 of
             (# s2, r #) -> (# s2, R1 r #)
    {-# INLINE greadAddr #-}
    gwriteAddr t (L1 x) addr s
      = case gconTags x of
          1## -> gwriteAddr 0## x (plusAddr# addr 4#)
                 (writeWord32OffAddr# addr 0# t s)
          _   -> gwriteAddr t x addr s
    gwriteAddr t xy@(R1 y) addr s
      = case (# gconTags y, plusWord# t (gconTags (undef1 @f xy)) #) of
          (# 1## , t' #) -> gwriteAddr 0## y (plusAddr# addr 4#)
                              (writeWord32OffAddr# addr 0# t' s)
          (# _   , t' #) -> gwriteAddr t' y addr s
    {-# INLINE gwriteAddr #-}
    gbyteSize xy = maxInt#
        (roundUpInt# 4# (gbyteAlign x) +# gbyteSize x)
        (roundUpInt# 4# (gbyteAlign y) +# gbyteSize y)
      where
        x = undef1 @f xy
        y = undef1 @g xy
    {-# INLINE gbyteSize #-}
    gbyteAlign xy = maxInt# 4# ( maxInt# (gbyteAlign (undef1 @f xy))
                                         (gbyteAlign (undef1 @g xy))
                               )
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags xy = gconTags (undef1 @f xy) `plusWord#` gconTags (undef1 @g xy)
    {-# INLINE gconTags #-}

maxInt# :: Int# -> Int# -> Int#
maxInt# a b | isTrue# (a ># b) = a
            | otherwise        = b

roundUpInt# :: Int# -> Int# -> Int#
roundUpInt# a b = case remInt# a b of
  0# -> a
  q  -> a +# b -# q
{-# INLINE roundUpInt# #-}

undef1 :: forall p q a . q a -> p a
undef1 = const undefined
{-# INLINE undef1 #-}



#if SIZEOF_HSWORD == 4
#define OFFSHIFT_W 2
#else
#define OFFSHIFT_W 3
#endif

instance GPrimBytes (URec Word) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSWORD# s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# (uWord# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UWord (indexWordArray# ba (uncheckedIShiftRL# off OFFSHIFT_W#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readWordArray# mba (uncheckedIShiftRL# off OFFSHIFT_W#) s of
          (# s1, r #) -> (# s1, UWord r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeWordArray# mba (uncheckedIShiftRL# off OFFSHIFT_W#) (uWord# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readWordOffAddr# a 0# s of (# s', x #) -> (# s', UWord x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeWordOffAddr# a 0# (uWord# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSWORD#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSWORD#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}

#if SIZEOF_HSINT == 4
#define OFFSHIFT_I 2
#else
#define OFFSHIFT_I 3
#endif

instance GPrimBytes (URec Int) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSINT# s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# (uInt# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UInt (indexIntArray# ba (uncheckedIShiftRL# off OFFSHIFT_I#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readIntArray# mba (uncheckedIShiftRL# off OFFSHIFT_I#) s of
          (# s1, r #) -> (# s1, UInt r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeIntArray# mba (uncheckedIShiftRL# off OFFSHIFT_I#) (uInt# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readIntOffAddr# a 0# s of (# s', x #) -> (# s', UInt x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeIntOffAddr# a 0# (uInt# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSINT#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSINT#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}


#if SIZEOF_HSFLOAT == 4
#define OFFSHIFT_F 2
#else
#define OFFSHIFT_F 3
#endif

instance GPrimBytes (URec Float) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSFLOAT# s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# (uFloat# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UFloat (indexFloatArray# ba (uncheckedIShiftRL# off OFFSHIFT_F#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readFloatArray# mba (uncheckedIShiftRL# off OFFSHIFT_F#) s of
          (# s1, r #) -> (# s1, UFloat r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeFloatArray# mba (uncheckedIShiftRL# off OFFSHIFT_F#) (uFloat# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readFloatOffAddr# a 0# s of (# s', x #) -> (# s', UFloat x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeFloatOffAddr# a 0# (uFloat# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSFLOAT#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSFLOAT#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}

#if SIZEOF_HSDOUBLE == 4
#define OFFSHIFT_D 2
#else
#define OFFSHIFT_D 3
#endif

instance GPrimBytes (URec Double) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSDOUBLE# s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# (uDouble# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UDouble (indexDoubleArray# ba (uncheckedIShiftRL# off OFFSHIFT_D#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readDoubleArray# mba (uncheckedIShiftRL# off OFFSHIFT_D#) s of
          (# s1, r #) -> (# s1, UDouble r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeDoubleArray# mba (uncheckedIShiftRL# off OFFSHIFT_D#) (uDouble# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readDoubleOffAddr# a 0# s of (# s', x #) -> (# s', UDouble x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeDoubleOffAddr# a 0# (uDouble# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSDOUBLE#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSDOUBLE#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}

#if SIZEOF_HSCHAR == 2
#define OFFSHIFT_C 1
#elif SIZEOF_HSCHAR == 4
#define OFFSHIFT_C 2
#else
#define OFFSHIFT_C 3
#endif

instance GPrimBytes (URec Char) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSCHAR# s0 of
         (# s1, marr #) -> case writeCharArray# marr 0# (uChar# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UChar (indexCharArray# ba (uncheckedIShiftRL# off OFFSHIFT_C#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readCharArray# mba (uncheckedIShiftRL# off OFFSHIFT_C#) s of
          (# s1, r #) -> (# s1, UChar r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeCharArray# mba (uncheckedIShiftRL# off OFFSHIFT_C#) (uChar# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readCharOffAddr# a 0# s of (# s', x #) -> (# s', UChar x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeCharOffAddr# a 0# (uChar# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSCHAR#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSCHAR#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}

#if SIZEOF_HSPTR == 4
#define OFFSHIFT_P 2
#else
#define OFFSHIFT_P 3
#endif

instance GPrimBytes (URec (Ptr ())) where
    ggetBytes x = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSPTR# s0 of
         (# s1, marr #) -> case writeAddrArray# marr 0# (uAddr# x) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE ggetBytes #-}
    gfromBytes _ off ba
      = UAddr (indexAddrArray# ba (uncheckedIShiftRL# off OFFSHIFT_P#))
    {-# INLINE gfromBytes #-}
    greadBytes _ mba off s
      = case readAddrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#) s of
          (# s1, r #) -> (# s1, UAddr r #)
    {-# INLINE greadBytes #-}
    gwriteBytes _ mba off x
      = writeAddrArray# mba (uncheckedIShiftRL# off OFFSHIFT_P#) (uAddr# x)
    {-# INLINE gwriteBytes #-}
    greadAddr _ a s
      = case readAddrOffAddr# a 0# s of (# s', x #) -> (# s', UAddr x #)
    {-# INLINE greadAddr #-}
    gwriteAddr _ x a
      = writeAddrOffAddr# a 0# (uAddr# x)
    {-# INLINE gwriteAddr #-}
    gbyteSize _ = SIZEOF_HSPTR#
    {-# INLINE gbyteSize #-}
    gbyteAlign _ = ALIGNMENT_HSPTR#
    {-# INLINE gbyteAlign #-}
    gbyteOffset _ = 0#
    {-# INLINE gbyteOffset #-}
    gconTags _ = 0##
    {-# INLINE gconTags #-}




--------------------------------------------------------------------------------
-- Basic instances
--------------------------------------------------------------------------------


instance PrimBytes Word where
    getBytes (W# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSWORD# s0 of
         (# s1, marr #) -> case writeWordArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = W# (indexWordArray# ba (uncheckedIShiftRL# off OFFSHIFT_W#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_W#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_W#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWordOffAddr# a 0# s of (# s', x #) -> (# s', W# x #)
    {-# INLINE readAddr #-}
    writeAddr (W# x) a
      = writeWordOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSWORD#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSWORD#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = W# (indexWordArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWordArray# mba i s of (# s', x #) -> (# s', W# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W# x) = writeWordArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes Int where
    getBytes (I# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSINT# s0 of
         (# s1, marr #) -> case writeIntArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = I# (indexIntArray# ba (uncheckedIShiftRL# off OFFSHIFT_I#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_I#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_I#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readIntOffAddr# a 0# s of (# s', x #) -> (# s', I# x #)
    {-# INLINE readAddr #-}
    writeAddr (I# x) a
      = writeIntOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSINT#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSINT#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = I# (indexIntArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readIntArray# mba i s of (# s', x #) -> (# s', I# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I# x) = writeIntArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Float where
    getBytes (F# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSFLOAT# s0 of
         (# s1, marr #) -> case writeFloatArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = F# (indexFloatArray# ba (uncheckedIShiftRL# off OFFSHIFT_F#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_F#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_F#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readFloatOffAddr# a 0# s of (# s', x #) -> (# s', F# x #)
    {-# INLINE readAddr #-}
    writeAddr (F# x) a
      = writeFloatOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSFLOAT#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSFLOAT#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = F# (indexFloatArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readFloatArray# mba i s of (# s', x #) -> (# s', F# x #)
    {-# INLINE readArray #-}
    writeArray mba i (F# x) = writeFloatArray# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Double where
    getBytes (D# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSDOUBLE# s0 of
         (# s1, marr #) -> case writeDoubleArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = D# (indexDoubleArray# ba (uncheckedIShiftRL# off OFFSHIFT_D#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_D#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_D#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readDoubleOffAddr# a 0# s of (# s', x #) -> (# s', D# x #)
    {-# INLINE readAddr #-}
    writeAddr (D# x) a
      = writeDoubleOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSDOUBLE#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSDOUBLE#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = D# (indexDoubleArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readDoubleArray# mba i s of (# s', x #) -> (# s', D# x #)
    {-# INLINE readArray #-}
    writeArray mba i (D# x) = writeDoubleArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes (Ptr a) where
    getBytes (Ptr x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_HSPTR# s0 of
         (# s1, marr #) -> case writeAddrArray# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = Ptr (indexAddrArray# ba (uncheckedIShiftRL# off OFFSHIFT_P#))
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off OFFSHIFT_P#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off OFFSHIFT_P#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readAddrOffAddr# a 0# s of (# s', x #) -> (# s', Ptr x #)
    {-# INLINE readAddr #-}
    writeAddr (Ptr x) a
      = writeAddrOffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_HSPTR#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_HSPTR#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = Ptr (indexAddrArray# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readAddrArray# mba i s of (# s', x #) -> (# s', Ptr x #)
    {-# INLINE readArray #-}
    writeArray mba i (Ptr x) = writeAddrArray# mba i x
    {-# INLINE writeArray #-}


instance PrimBytes Int8 where
    getBytes (I8# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT8# s0 of
         (# s1, marr #) -> case writeInt8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba = indexArray ba off
    {-# INLINE fromBytes #-}
    readBytes = readArray
    {-# INLINE readBytes #-}
    writeBytes = writeArray
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt8OffAddr# a 0# s of (# s', x #) -> (# s', I8# x #)
    {-# INLINE readAddr #-}
    writeAddr (I8# x) a
      = writeInt8OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT8#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT8#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = I8# (indexInt8Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt8Array# mba i s of (# s', x #) -> (# s', I8# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I8# x) = writeInt8Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int16 where
    getBytes (I16# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT16# s0 of
         (# s1, marr #) -> case writeInt16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 1#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt16OffAddr# a 0# s of (# s', x #) -> (# s', I16# x #)
    {-# INLINE readAddr #-}
    writeAddr (I16# x) a
      = writeInt16OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT16#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT16#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = I16# (indexInt16Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt16Array# mba i s of (# s', x #) -> (# s', I16# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I16# x) = writeInt16Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int32 where
    getBytes (I32# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT32# s0 of
         (# s1, marr #) -> case writeInt32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 2#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt32OffAddr# a 0# s of (# s', x #) -> (# s', I32# x #)
    {-# INLINE readAddr #-}
    writeAddr (I32# x) a
      = writeInt32OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT32#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT32#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = I32# (indexInt32Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt32Array# mba i s of (# s', x #) -> (# s', I32# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I32# x) = writeInt32Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Int64 where
    getBytes (I64# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_INT64# s0 of
         (# s1, marr #) -> case writeInt64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 3#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readInt64OffAddr# a 0# s of (# s', x #) -> (# s', I64# x #)
    {-# INLINE readAddr #-}
    writeAddr (I64# x) a
      = writeInt64OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_INT64#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_INT64#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = I64# (indexInt64Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readInt64Array# mba i s of (# s', x #) -> (# s', I64# x #)
    {-# INLINE readArray #-}
    writeArray mba i (I64# x) = writeInt64Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word8 where
    getBytes (W8# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD8# s0 of
         (# s1, marr #) -> case writeWord8Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba = indexArray ba off
    {-# INLINE fromBytes #-}
    readBytes = readArray
    {-# INLINE readBytes #-}
    writeBytes = writeArray
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord8OffAddr# a 0# s of (# s', x #) -> (# s', W8# x #)
    {-# INLINE readAddr #-}
    writeAddr (W8# x) a
      = writeWord8OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD8#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD8#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = W8# (indexWord8Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord8Array# mba i s of (# s', x #) -> (# s', W8# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W8# x) = writeWord8Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word16 where
    getBytes (W16# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD16# s0 of
         (# s1, marr #) -> case writeWord16Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 1#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 1#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord16OffAddr# a 0# s of (# s', x #) -> (# s', W16# x #)
    {-# INLINE readAddr #-}
    writeAddr (W16# x) a
      = writeWord16OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD16#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD16#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = W16# (indexWord16Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord16Array# mba i s of (# s', x #) -> (# s', W16# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W16# x) = writeWord16Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word32 where
    getBytes (W32# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD32# s0 of
         (# s1, marr #) -> case writeWord32Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 2#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 2#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord32OffAddr# a 0# s of (# s', x #) -> (# s', W32# x #)
    {-# INLINE readAddr #-}
    writeAddr (W32# x) a
      = writeWord32OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD32#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD32#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = W32# (indexWord32Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord32Array# mba i s of (# s', x #) -> (# s', W32# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W32# x) = writeWord32Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes Word64 where
    getBytes (W64# x) = case runRW#
      ( \s0 -> case newByteArray# SIZEOF_WORD64# s0 of
         (# s1, marr #) -> case writeWord64Array# marr 0# x s1 of
             s2 -> unsafeFreezeByteArray# marr s2
      ) of (# _, a #) -> a
    {-# NOINLINE getBytes #-}
    fromBytes off ba
      = indexArray ba (uncheckedIShiftRL# off 3#)
    {-# INLINE fromBytes #-}
    readBytes mba off
      = readArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE readBytes #-}
    writeBytes mba off
      = writeArray mba (uncheckedIShiftRL# off 3#)
    {-# INLINE writeBytes #-}
    readAddr a s
      = case readWord64OffAddr# a 0# s of (# s', x #) -> (# s', W64# x #)
    {-# INLINE readAddr #-}
    writeAddr (W64# x) a
      = writeWord64OffAddr# a 0# x
    {-# INLINE writeAddr #-}
    byteSize _ = SIZEOF_WORD64#
    {-# INLINE byteSize #-}
    byteAlign _ = ALIGNMENT_WORD64#
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba i = W64# (indexWord64Array# ba i)
    {-# INLINE indexArray #-}
    readArray mba i s
      = case readWord64Array# mba i s of (# s', x #) -> (# s', W64# x #)
    {-# INLINE readArray #-}
    writeArray mba i (W64# x) = writeWord64Array# mba i x
    {-# INLINE writeArray #-}

instance PrimBytes (Idx x) where
    getBytes = unsafeCoerce# (getBytes @Word)
    {-# INLINE getBytes #-}
    fromBytes  = unsafeCoerce# (fromBytes @Word)
    {-# INLINE fromBytes #-}
    readBytes = unsafeCoerce# (readBytes @Word)
    {-# INLINE readBytes #-}
    writeBytes = unsafeCoerce# (writeBytes @Word)
    {-# INLINE writeBytes #-}
    readAddr = unsafeCoerce# (readAddr @Word)
    {-# INLINE readAddr #-}
    writeAddr = unsafeCoerce# (readAddr @Word)
    {-# INLINE writeAddr #-}
    byteSize = unsafeCoerce# (byteSize @Word)
    {-# INLINE byteSize #-}
    byteAlign = unsafeCoerce# (byteAlign @Word)
    {-# INLINE byteAlign #-}
    byteOffset = unsafeCoerce# (byteOffset @Word)
    {-# INLINE byteOffset #-}
    indexArray = unsafeCoerce# (indexArray @Word)
    {-# INLINE indexArray #-}
    readArray = unsafeCoerce# (readArray @Word)
    {-# INLINE readArray #-}
    writeArray = unsafeCoerce# (writeArray @Word)
    {-# INLINE writeArray #-}

instance RepresentableList xs => PrimBytes (Idxs xs) where
    getBytes is = case runRW#
       ( \s0 -> case newByteArray# (byteSize is) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (writeBytes marr 0# is s1)
       ) of (# _, a #) -> a
    {-# INLINE getBytes #-}
    fromBytes off ba = unsafeCoerce#
        (go (uncheckedIShiftRL# off OFFSHIFT_W#) (unsafeCoerce# (tList @_ @xs)))
      where
        go _ []           = []
        go i (Proxy : ls) = W# (indexWordArray# ba i) : go (i +# 1#) ls
    {-# INLINE fromBytes #-}
    readBytes mba off s = unsafeCoerce#
        (go (uncheckedIShiftRL# off OFFSHIFT_W#) (unsafeCoerce# (tList @_ @xs)) s)
      where
        go _ [] s0 = (# s0, [] #)
        go i (Proxy : ls) s0 = case readWordArray# mba off s0 of
          (# s1, w #) -> case go (i +# 1#) ls s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readBytes #-}
    writeBytes mba off is
        = go (uncheckedIShiftRL# off OFFSHIFT_W#) (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# 1#) xs (writeWordArray# mba i x s)
    {-# INLINE writeBytes #-}
    readAddr addr s = unsafeCoerce#
        (go addr (unsafeCoerce# (tList @_ @xs)) s)
      where
        go _ [] s0 = (# s0, [] #)
        go i (Proxy : ls) s0 = case readWordOffAddr# i 0# s0 of
          (# s1, w #) -> case go (plusAddr# i SIZEOF_HSWORD#) ls s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readAddr #-}
    writeAddr is addr
        = go addr (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (plusAddr# i SIZEOF_HSWORD#) xs
                               (writeWordOffAddr# i 0# x s)
    {-# INLINE writeAddr #-}
    byteSize _ = case dimVal (order' @xs) of
      W# n -> byteSize (undefined :: Idx x) *# word2Int# n
    {-# INLINE byteSize #-}
    byteAlign _ = byteAlign (undefined :: Idx x)
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}
    indexArray ba off
      | n@(W# n#) <- dimVal (order' @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n)
      where
        go _ 0 = []
        go i n = W# (indexWordArray# ba i) : go (i +# 1#) (n-1)
    {-# INLINE indexArray #-}
    readArray mba off s
      | n@(W# n#) <- dimVal (order' @xs)
        = unsafeCoerce# (go (off *# word2Int# n#) n s)
      where
        go _ 0 s0 = (# s0, [] #)
        go i n s0 = case readWordArray# mba off s0 of
          (# s1, w #) -> case go (i +# 1#) (n-1) s1 of
             (# s2, xs #) -> (# s2, W# w : xs #)
    {-# INLINE readArray #-}
    writeArray mba off is
      | W# n# <- dimVal (order' @xs)
        = go (off *# word2Int# n#) (listIdxs is)
      where
        go _ [] s         = s
        go i (W# x :xs) s = go (i +# 1#) xs (writeWordArray# mba i x s)
    {-# INLINE writeArray #-}

instance ( RepresentableList xs
         , L.All PrimBytes xs
         ) => PrimBytes (TL.Tuple xs) where
    getBytes   = unsafeCoerce# (getBytes @(TS.Tuple xs))
    {-# INLINE getBytes #-}
    fromBytes  = unsafeCoerce# (fromBytes @(TS.Tuple xs))
    {-# INLINE fromBytes #-}
    readBytes  = unsafeCoerce# (readBytes @(TS.Tuple xs))
    {-# INLINE readBytes #-}
    writeBytes = unsafeCoerce# (writeBytes @(TS.Tuple xs))
    {-# INLINE writeBytes #-}
    readAddr   = unsafeCoerce# (readAddr  @(TS.Tuple xs))
    {-# INLINE readAddr  #-}
    writeAddr  = unsafeCoerce# (writeAddr  @(TS.Tuple xs))
    {-# INLINE writeAddr  #-}
    byteSize   = unsafeCoerce# (byteSize @(TS.Tuple xs))
    {-# INLINE byteSize #-}
    byteAlign  = unsafeCoerce# (byteAlign @(TS.Tuple xs))
    {-# INLINE byteAlign #-}
    byteOffset = unsafeCoerce# (byteOffset @(TS.Tuple xs))
    {-# INLINE byteOffset #-}
    indexArray = unsafeCoerce# (indexArray @(TS.Tuple xs))
    {-# INLINE indexArray #-}
    readArray  = unsafeCoerce# (readArray @(TS.Tuple xs))
    {-# INLINE readArray #-}
    writeArray = unsafeCoerce# (writeArray @(TS.Tuple xs))
    {-# INLINE writeArray #-}

instance ( RepresentableList xs
         , L.All PrimBytes xs
         ) => PrimBytes (TS.Tuple xs) where
    getBytes tup = case runRW#
        ( \s0 -> case newByteArray# (byteSize tup) s0 of
           (# s1, marr #) -> unsafeFreezeByteArray# marr
             (go marr 0# tup (types tup) s1)
        ) of (# _, a #) -> a
      where
        go :: L.All PrimBytes ds => MutableByteArray# s
          -> Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ _ Empty s = s
        go mb n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go mb (n' +# byteSize x) xs ts (writeBytes mb n' x s)
    {-# INLINE getBytes #-}
    fromBytes off ba = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> TS.Tuple ds
        go _ Empty = Empty
        go n (t :* ts@TypeList)
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = TS.Id (fromBytes (off +# n') ba) :* go (n' +# byteSize x) ts
    {-# INLINE fromBytes #-}
    readBytes mb off = go mb 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => MutableByteArray# s
           -> Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ _ Empty s0 = (# s0, Empty #)
        go mba n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = case readBytes mba (off +# n') s0 of
              (# s1, r #) -> case go mba (n' +# byteSize x) ts s1 of
                (# s2, rs #) -> (# s2, TS.Id r :* rs #)
    {-# INLINE readBytes #-}
    writeBytes mba off tup = go mba 0# tup (types tup)
      where
        go :: L.All PrimBytes ds => MutableByteArray# s
           -> Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ _ Empty s = s
        go mb n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go mb (n' +# byteSize x) xs ts (writeBytes mb (off +# n') x s)
    {-# INLINE writeBytes #-}
    readAddr addr = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ds
           => Int# -> TypeList ds -> State# s -> (# State# s, TS.Tuple ds #)
        go _ Empty s0 = (# s0, Empty #)
        go n (t :* ts@TypeList) s0
          | x <- undefP t
          , n' <- roundUpInt# n (byteAlign x)
          = case readAddr (plusAddr# addr n') s0 of
              (# s1, r #) -> case go (n' +# byteSize x) ts s1 of
                (# s2, rs #) -> (# s2, TS.Id r :* rs #)
    {-# INLINE readAddr #-}
    writeAddr tup addr = go 0# tup (types tup)
      where
        go :: L.All PrimBytes ds
           => Int# -> TS.Tuple ds -> TypeList ds -> State# s -> State# s
        go _ _ Empty s = s
        go n (TS.Id x :* xs) (_ :* ts@TypeList) s
          | n' <- roundUpInt# n (byteAlign x)
          = go (n' +# byteSize x) xs ts (writeAddr x (plusAddr# addr n') s)
    {-# INLINE writeAddr #-}
    byteSize _  = go 0# (tList @_ @xs)
      where
        go :: L.All PrimBytes ys => Int# -> TypeList ys -> Int#
        go s Empty     = s
        go s (p :* ps) = let x = undefP p
                         in  go (roundUpInt# s (byteAlign x) +# byteSize x) ps
    {-# INLINE byteSize #-}
    byteAlign _ = go (tList @_ @xs)
      where
        go :: L.All PrimBytes ys => TypeList ys -> Int#
        go Empty     = 0#
        go (p :* ps) = maxInt# (byteAlign (undefP p)) (go ps)
    {-# INLINE byteAlign #-}
    byteOffset _ = 0#
    {-# INLINE byteOffset #-}


undefP :: Proxy p -> p
undefP = const undefined
{-# INLINE undefP #-}


instance PrimBytes a => PrimBytes (Maybe a)
instance (PrimBytes a, PrimBytes b) => PrimBytes (Either a b)
instance PrimBytes a => PrimBytes [a] -- ??? likely to give inf byteSize


data PrimTag a where
    PTagFloat  :: PrimTag Float
    PTagDouble :: PrimTag Double
    PTagInt    :: PrimTag Int
    PTagInt8   :: PrimTag Int8
    PTagInt16  :: PrimTag Int16
    PTagInt32  :: PrimTag Int32
    PTagInt64  :: PrimTag Int64
    PTagWord   :: PrimTag Word
    PTagWord8  :: PrimTag Word8
    PTagWord16 :: PrimTag Word16
    PTagWord32 :: PrimTag Word32
    PTagWord64 :: PrimTag Word64
    PTagPtr    :: PrimTag (Ptr a)
    PTagOther  :: PrimTag a

class PrimTagged a where
    primTag' :: a -> PrimTag a

-- | This function allows to find out a type by comparing its tag.
--   This is needed for array overloading, to infer array instances.
--   For non-basic types it defaults to `PTagOther`
primTag :: PrimBytes a => a -> PrimTag a
primTag = primTag'
{-# INLINE primTag #-}

instance {-# OVERLAPPABLE #-} PrimTagged a where
    primTag' = const PTagOther
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Float where
    primTag' = const PTagFloat
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Double where
    primTag' = const PTagDouble
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int where
    primTag' = const PTagInt
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int8 where
    primTag' = const PTagInt8
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int16 where
    primTag' = const PTagInt16
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int32 where
    primTag' = const PTagInt32
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Int64 where
    primTag' = const PTagInt64
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word where
    primTag' = const PTagWord
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word8 where
    primTag' = const PTagWord8
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word16 where
    primTag' = const PTagWord16
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word32 where
    primTag' = const PTagWord32
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged Word64 where
    primTag' = const PTagWord64
    {-# INLINE primTag' #-}

instance {-# OVERLAPPING #-} PrimTagged (Ptr a) where
    primTag' = const PTagPtr
    {-# INLINE primTag' #-}
