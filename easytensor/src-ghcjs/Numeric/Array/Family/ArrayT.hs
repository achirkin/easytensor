{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
-- {-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE GHCForeignImportPrim  #-}
{-# LANGUAGE UnliftedFFITypes      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Numeric.Array.Family.ArrayT () where


import           GHC.Base                  (runRW#)
import           GHC.Int   (Int16 (..), Int32 (..), Int8 (..))
import           GHC.Word  (Word16 (..), Word32 (..), Word8 (..))
import           GHC.Prim
import           GHC.Types                 (Float (..), Double (..), Int (..), Word (..))
--import           GHCJS.Types

import           Numeric.Array.ElementWise
import           Numeric.Array.Family
import           Numeric.Commons
import           Numeric.DataFrame.Type
import           Numeric.Dimensions
import           Numeric.Dimensions.Traverse
import           Numeric.TypeLits
import           Numeric.Matrix.Type


type instance ElemRep  (ArrayT t      ds) = ElemRep t
type instance ElemPrim (ArrayT Float  ds) = Float#
type instance ElemPrim (ArrayT Double ds) = Double#
type instance ElemPrim (ArrayT Int    ds) = Int#
type instance ElemPrim (ArrayT Int8   ds) = Int#
type instance ElemPrim (ArrayT Int16  ds) = Int#
type instance ElemPrim (ArrayT Int32  ds) = Int#
type instance ElemPrim (ArrayT Word   ds) = Word#
type instance ElemPrim (ArrayT Word8  ds) = Word#
type instance ElemPrim (ArrayT Word16 ds) = Word#
type instance ElemPrim (ArrayT Word32 ds) = Word#
type instance ElemPrim (ArrayT Word8Clamped ds) = Int#


instance Dimensions ds => PrimBytes (ArrayT Float ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapFloatArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Float)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Float)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Float)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetFloat#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Double ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapDoubleArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Double)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Double)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Double)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetDouble#
  {-# INLINE ix #-}


instance Dimensions ds => PrimBytes (ArrayT Int ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapIntArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Int)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Int)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int8 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt8ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int8)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Int8)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Int8)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt8#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int16 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt16ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int16)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Int16)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Int16)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt16#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Int32 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapInt32ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Int32)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Int32)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Int32)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetInt32#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWordArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Word)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Word)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word8 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord8ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word8)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Word8)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Word8)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord8#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word8Clamped ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord8ClampedArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word8Clamped)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Word8Clamped)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Word8Clamped)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord8Clamped#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word16 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord16ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word16)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Word16)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Word16)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord16#
  {-# INLINE ix #-}

instance Dimensions ds => PrimBytes (ArrayT Word32 ds) where
  toBytes v = (# js_byteOffset v `quotInt#` elementByteSize v , js_length v , js_wrapArrayT v #)
  {-# INLINE toBytes #-}
  fromBytes (# off, len, arr #) = js_unwrapWord32ArrayOffLen arr off len
  {-# INLINE fromBytes #-}
  byteSize ~_ = case totalDim (dim @ds) of I# n -> n *# byteSize (undefined :: Word32)
  {-# INLINE byteSize #-}
  byteAlign ~_ = byteAlign (undefined :: Word32)
  {-# INLINE byteAlign #-}
  elementByteSize ~_ = byteSize (undefined :: Word32)
  {-# INLINE elementByteSize #-}
  ix = js_indexArrayOffsetWord32#
  {-# INLINE ix #-}




instance ( Dimensions ds
         , Show t
         , ElementWise (Idx ds) t (ArrayT t ds)
         )
      => Show (ArrayT t (ds :: [Nat])) where
  show x = case dim @ds of
    D -> "{ " ++ show (x ! Z) ++ " }"
    Dn :* D -> ('{' :) . drop 1 $
                    foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                            [minBound .. maxBound]
    (Dn :: Dim (n :: Nat)) :* (Dn :: Dim (m :: Nat)) :* (_ :: Dim (dss :: [Nat])) ->
      case inferDropNDimensions @2 @ds of
        Evidence ->
          let loopInner :: Idx dss -> Idx '[n,m] -> String
              loopInner ods (n:!m:!_) = ('{' :) . drop 2 $
                              foldr (\i ss -> '\n':
                                      foldr (\j s ->
                                               ", " ++ show (x ! (i :! j :! ods)) ++ s
                                            ) ss [1..m]
                                    ) " }" [1..n]
              loopOuter ::  Idx dss -> String -> String
              loopOuter Z s  = "\n" ++ loopInner Z maxBound ++ s
              loopOuter ds s = "\n(i j" ++ drop 3 (show ds) ++ "):\n"
                                    ++ loopInner ds maxBound ++ s
          in drop 1 $ foldr loopOuter "" [minBound..maxBound]


instance Eq (ArrayT t ds) where
    (==) = js_arrayTEq
    (/=) = js_arrayTNEq
foreign import javascript unsafe "$1.every(function (e, i) { return e == $2[i]; })" js_arrayTEq  :: ArrayT t ds -> ArrayT t ds -> Bool
foreign import javascript unsafe "$1.some(function (e, i) { return e !== $2[i]; })" js_arrayTNEq :: ArrayT t ds -> ArrayT t ds -> Bool




instance Ord (ArrayT t ds) where
    (<)  = js_arrayTLT
    (<=) = js_arrayTLE
    (>)  = js_arrayTGT
    (>=) = js_arrayTGE
    compare a b = case js_arrayTCmp a b of
        1 -> GT
        0 -> EQ
        _ -> LT
foreign import javascript unsafe "$1.every(function (e, i) { return e <  $2[i]; })" js_arrayTLT  :: ArrayT t ds -> ArrayT t ds -> Bool
foreign import javascript unsafe "$1.every(function (e, i) { return e <= $2[i]; })" js_arrayTLE  :: ArrayT t ds -> ArrayT t ds -> Bool
foreign import javascript unsafe "$1.every(function (e, i) { return e >  $2[i]; })" js_arrayTGT  :: ArrayT t ds -> ArrayT t ds -> Bool
foreign import javascript unsafe "$1.every(function (e, i) { return e >= $2[i]; })" js_arrayTGE  :: ArrayT t ds -> ArrayT t ds -> Bool
foreign import javascript unsafe "$1.reduce(function (r, e, i) { return r === 0 ? (e > $2[i] ? 1 : (e < $2[i] ? -1 : 0)) : r;}, 0)" js_arrayTCmp :: ArrayT t ds -> ArrayT t ds -> Int


instance Dimensions ds => Num (ArrayT Float ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewFloatArray (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Double ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewDoubleArray (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Int ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewIntArray (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Int8 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewInt8Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Int16 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewInt16Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Int32 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewInt32Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Word ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewWordArray (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Word8 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewWord8Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Word16 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewWord16Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Word32 ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewWord32Array (totalDim (dim @ds)) . fromInteger
instance Dimensions ds => Num (ArrayT Word8Clamped ds) where
    (+) = js_arrayTPlus
    (-) = js_arrayTMinus
    (*) = js_arrayTTimes
    negate = js_arrayTNegate
    abs    = js_arrayTAbs
    signum = js_arrayTSignum
    fromInteger = js_fillNewWord8ClampedArray (totalDim (dim @ds)) . fromInteger

foreign import javascript unsafe "$1.map(function (e, i) { return e + $2[i]; })" js_arrayTPlus   :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e, i) { return e - $2[i]; })" js_arrayTMinus  :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e, i) { return e * $2[i]; })" js_arrayTTimes  :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return -e; })"           js_arrayTNegate :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.abs(e); })"  js_arrayTAbs    :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.sign(e); })" js_arrayTSignum :: ArrayT t ds -> ArrayT t ds



instance Dimensions ds => Fractional (ArrayT Float ds) where
    recip = js_arrayTRecip
    (/)   = js_arrayTDivide
    fromRational = js_fillNewFloatArray (totalDim (dim @ds)) . fromRational
instance Dimensions ds => Fractional (ArrayT Double ds) where
    recip = js_arrayTRecip
    (/)   = js_arrayTDivide
    fromRational = js_fillNewDoubleArray (totalDim (dim @ds)) . fromRational

foreign import javascript unsafe "$1.map(function (e, i) { return e/$2[i]; })" js_arrayTDivide :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return 1/e; })"        js_arrayTRecip  :: ArrayT t ds -> ArrayT t ds


instance Dimensions ds => Floating (ArrayT Float ds) where
  pi = broadcast pi
  {-# INLINE pi #-}
  exp = js_arrayTexp
  {-# INLINE exp #-}
  log = js_arrayTlog
  {-# INLINE log #-}
  sqrt = js_arrayTsqrt
  {-# INLINE sqrt #-}
  sin = js_arrayTsin
  {-# INLINE sin #-}
  cos = js_arrayTcos
  {-# INLINE cos #-}
  tan = js_arrayTtan
  {-# INLINE tan #-}
  asin = js_arrayTasin
  {-# INLINE asin #-}
  acos = js_arrayTacos
  {-# INLINE acos #-}
  atan = js_arrayTatan
  {-# INLINE atan #-}
  sinh = js_arrayTsinh
  {-# INLINE sinh #-}
  cosh = js_arrayTcosh
  {-# INLINE cosh #-}
  tanh = js_arrayTtanh
  {-# INLINE tanh #-}
  (**) = js_arrayTpower
  {-# INLINE (**) #-}
  logBase = js_arrayTlogBase
  {-# INLINE logBase #-}
  asinh = js_arrayTasinh
  {-# INLINE asinh #-}
  acosh = js_arrayTacosh
  {-# INLINE acosh #-}
  atanh = js_arrayTatanh
  {-# INLINE atanh #-}

instance Dimensions ds => Floating (ArrayT Double ds) where
  pi = broadcast pi
  {-# INLINE pi #-}
  exp = js_arrayTexp
  {-# INLINE exp #-}
  log = js_arrayTlog
  {-# INLINE log #-}
  sqrt = js_arrayTsqrt
  {-# INLINE sqrt #-}
  sin = js_arrayTsin
  {-# INLINE sin #-}
  cos = js_arrayTcos
  {-# INLINE cos #-}
  tan = js_arrayTtan
  {-# INLINE tan #-}
  asin = js_arrayTasin
  {-# INLINE asin #-}
  acos = js_arrayTacos
  {-# INLINE acos #-}
  atan = js_arrayTatan
  {-# INLINE atan #-}
  sinh = js_arrayTsinh
  {-# INLINE sinh #-}
  cosh = js_arrayTcosh
  {-# INLINE cosh #-}
  tanh = js_arrayTtanh
  {-# INLINE tanh #-}
  (**) = js_arrayTpower
  {-# INLINE (**) #-}
  logBase = js_arrayTlogBase
  {-# INLINE logBase #-}
  asinh = js_arrayTasinh
  {-# INLINE asinh #-}
  acosh = js_arrayTacosh
  {-# INLINE acosh #-}
  atanh = js_arrayTatanh
  {-# INLINE atanh #-}


foreign import javascript unsafe "$1.map(function (e) { return Math.exp(e); })"   js_arrayTexp :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.log(e); })"   js_arrayTlog :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.sqrt(e); })"  js_arrayTsqrt :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.sin(e); })"   js_arrayTsin :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.cos(e); })"   js_arrayTcos :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.tan(e); })"   js_arrayTtan :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.asin(e); })"  js_arrayTasin :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.acos(e); })"  js_arrayTacos :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.atan(e); })"  js_arrayTatan :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.sinh(e); })"  js_arrayTsinh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.cosh(e); })"  js_arrayTcosh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.tanh(e); })"  js_arrayTtanh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.asinh(e); })" js_arrayTasinh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.acosh(e); })" js_arrayTacosh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e) { return Math.atanh(e); })" js_arrayTatanh :: ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e,i) { return Math.log($2[i])/Math.log(e); })" js_arrayTlogBase :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds
foreign import javascript unsafe "$1.map(function (e,i) { return Math.pow(e,$2[i]); })" js_arrayTpower :: ArrayT t ds -> ArrayT t ds -> ArrayT t ds



instance Dimensions ds => Bounded (ArrayT Int ds) where
  maxBound = js_fillNewIntArray (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewIntArray (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Int8 ds) where
  maxBound = js_fillNewInt8Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewInt8Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Int16 ds) where
  maxBound = js_fillNewInt16Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewInt16Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Int32 ds) where
  maxBound = js_fillNewInt32Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewInt32Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Word ds) where
  maxBound = js_fillNewWordArray (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewWordArray (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Word8 ds) where
  maxBound = js_fillNewWord8Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewWord8Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Word16 ds) where
  maxBound = js_fillNewWord16Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewWord16Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Word32 ds) where
  maxBound = js_fillNewWord32Array (totalDim (dim @ds)) maxBound
  {-# INLINE maxBound #-}
  minBound = js_fillNewWord32Array (totalDim (dim @ds)) minBound
  {-# INLINE minBound #-}
instance Dimensions ds => Bounded (ArrayT Word8Clamped ds) where
  maxBound = js_fillNewWord8ClampedArray (totalDim (dim @ds)) 255
  {-# INLINE maxBound #-}
  minBound = js_fillNewWord8ClampedArray (totalDim (dim @ds)) 0
  {-# INLINE minBound #-}






wr :: (State# RealWorld -> (# State# RealWorld, MutableArrayT RealWorld t ds #) )
   -> (MutableArrayT RealWorld t ds -> State# RealWorld -> State# RealWorld)
   -> ArrayT t ds
wr fma ff = case runRW#
     ( \s0 -> case fma s0 of
          (# s1, ma #) -> case ff ma s1 of s2 -> unsafeFreezeArrayT# ma s2
     ) of (# _, r #) -> r
{-# INLINE wr #-}



instance Dimensions ds => ElementWise (Idx ds) Float (ArrayT Float ds) where
    indexOffset# x i = js_indexArrayOffsetFloat i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetFloat j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewFloatArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (F# v) = case fromEnum i of I# j -> js_setArrayOffsetFloat# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createFloatArray (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetFloat off x) of
                      (F# r) -> js_writeArrayOffsetFloat# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createFloatArray n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (F# r) -> js_writeArrayOffsetFloat# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createFloatArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(F# z) u a s -> js_writeArrayOffsetFloat# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetFloat off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetFloat off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createFloatArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(F# z) u a s -> js_writeArrayOffsetFloat# off z a (u a s) ) <$> f i (js_indexArrayOffsetFloat off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createFloatArray n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(F# z) u a s -> js_writeArrayOffsetFloat# off z a (u a s) ) <$> f (js_indexArrayOffsetFloat off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Double (ArrayT Double ds) where
    indexOffset# x i = js_indexArrayOffsetDouble i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetDouble j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewDoubleArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (D# v) = case fromEnum i of I# j -> js_setArrayOffsetDouble# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createDoubleArray (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetDouble off x) of
                      (D# r) -> js_writeArrayOffsetDouble# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createDoubleArray n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (D# r) -> js_writeArrayOffsetDouble# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createDoubleArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(D# z) u a s -> js_writeArrayOffsetDouble# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetDouble off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetDouble off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createDoubleArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(D# z) u a s -> js_writeArrayOffsetDouble# off z a (u a s) ) <$> f i (js_indexArrayOffsetDouble off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createDoubleArray n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(D# z) u a s -> js_writeArrayOffsetDouble# off z a (u a s) ) <$> f (js_indexArrayOffsetDouble off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Int32 (ArrayT Int32 ds) where
    indexOffset# x i = js_indexArrayOffsetInt32 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetInt32 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewInt32Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (I32# v) = case fromEnum i of I# j -> js_setArrayOffsetInt32# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createInt32Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetInt32 off x) of
                      (I32# r) -> js_writeArrayOffsetInt32# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createInt32Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (I32# r) -> js_writeArrayOffsetInt32# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createInt32Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I32# z) u a s -> js_writeArrayOffsetInt32# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetInt32 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetInt32 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createInt32Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I32# z) u a s -> js_writeArrayOffsetInt32# off z a (u a s) ) <$> f i (js_indexArrayOffsetInt32 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createInt32Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(I32# z) u a s -> js_writeArrayOffsetInt32# off z a (u a s) ) <$> f (js_indexArrayOffsetInt32 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

instance Dimensions ds => ElementWise (Idx ds) Int16 (ArrayT Int16 ds) where
    indexOffset# x i = js_indexArrayOffsetInt16 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetInt16 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewInt16Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (I16# v) = case fromEnum i of I# j -> js_setArrayOffsetInt16# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createInt16Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetInt16 off x) of
                      (I16# r) -> js_writeArrayOffsetInt16# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createInt16Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (I16# r) -> js_writeArrayOffsetInt16# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createInt16Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I16# z) u a s -> js_writeArrayOffsetInt16# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetInt16 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetInt16 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createInt16Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I16# z) u a s -> js_writeArrayOffsetInt16# off z a (u a s) ) <$> f i (js_indexArrayOffsetInt16 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createInt16Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(I16# z) u a s -> js_writeArrayOffsetInt16# off z a (u a s) ) <$> f (js_indexArrayOffsetInt16 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Int8 (ArrayT Int8 ds) where
    indexOffset# x i = js_indexArrayOffsetInt8 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetInt8 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewInt8Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (I8# v) = case fromEnum i of I# j -> js_setArrayOffsetInt8# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createInt8Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetInt8 off x) of
                      (I8# r) -> js_writeArrayOffsetInt8# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createInt8Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (I8# r) -> js_writeArrayOffsetInt8# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createInt8Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I8# z) u a s -> js_writeArrayOffsetInt8# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetInt8 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetInt8 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createInt8Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I8# z) u a s -> js_writeArrayOffsetInt8# off z a (u a s) ) <$> f i (js_indexArrayOffsetInt8 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createInt8Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(I8# z) u a s -> js_writeArrayOffsetInt8# off z a (u a s) ) <$> f (js_indexArrayOffsetInt8 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Int (ArrayT Int ds) where
    indexOffset# x i = js_indexArrayOffsetInt i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetInt j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewIntArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (I# v) = case fromEnum i of I# j -> js_setArrayOffsetInt# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createIntArray (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetInt off x) of
                      (I# r) -> js_writeArrayOffsetInt# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createIntArray n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (I# r) -> js_writeArrayOffsetInt# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createIntArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I# z) u a s -> js_writeArrayOffsetInt# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetInt off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetInt off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createIntArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(I# z) u a s -> js_writeArrayOffsetInt# off z a (u a s) ) <$> f i (js_indexArrayOffsetInt off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createIntArray n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(I# z) u a s -> js_writeArrayOffsetInt# off z a (u a s) ) <$> f (js_indexArrayOffsetInt off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds




instance Dimensions ds => ElementWise (Idx ds) Word32 (ArrayT Word32 ds) where
    indexOffset# x i = js_indexArrayOffsetWord32 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetWord32 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewWord32Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (W32# v) = case fromEnum i of I# j -> js_setArrayOffsetWord32# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createWord32Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetWord32 off x) of
                      (W32# r) -> js_writeArrayOffsetWord32# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createWord32Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (W32# r) -> js_writeArrayOffsetWord32# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createWord32Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W32# z) u a s -> js_writeArrayOffsetWord32# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetWord32 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetWord32 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createWord32Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W32# z) u a s -> js_writeArrayOffsetWord32# off z a (u a s) ) <$> f i (js_indexArrayOffsetWord32 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createWord32Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(W32# z) u a s -> js_writeArrayOffsetWord32# off z a (u a s) ) <$> f (js_indexArrayOffsetWord32 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

instance Dimensions ds => ElementWise (Idx ds) Word16 (ArrayT Word16 ds) where
    indexOffset# x i = js_indexArrayOffsetWord16 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetWord16 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewWord16Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (W16# v) = case fromEnum i of I# j -> js_setArrayOffsetWord16# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createWord16Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetWord16 off x) of
                      (W16# r) -> js_writeArrayOffsetWord16# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createWord16Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (W16# r) -> js_writeArrayOffsetWord16# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createWord16Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W16# z) u a s -> js_writeArrayOffsetWord16# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetWord16 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetWord16 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createWord16Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W16# z) u a s -> js_writeArrayOffsetWord16# off z a (u a s) ) <$> f i (js_indexArrayOffsetWord16 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createWord16Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(W16# z) u a s -> js_writeArrayOffsetWord16# off z a (u a s) ) <$> f (js_indexArrayOffsetWord16 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Word8 (ArrayT Word8 ds) where
    indexOffset# x i = js_indexArrayOffsetWord8 i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetWord8 j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewWord8Array (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (W8# v) = case fromEnum i of I# j -> js_setArrayOffsetWord8# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createWord8Array (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetWord8 off x) of
                      (W8# r) -> js_writeArrayOffsetWord8# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createWord8Array n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (W8# r) -> js_writeArrayOffsetWord8# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createWord8Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W8# z) u a s -> js_writeArrayOffsetWord8# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetWord8 off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetWord8 off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createWord8Array n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W8# z) u a s -> js_writeArrayOffsetWord8# off z a (u a s) ) <$> f i (js_indexArrayOffsetWord8 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createWord8Array n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(W8# z) u a s -> js_writeArrayOffsetWord8# off z a (u a s) ) <$> f (js_indexArrayOffsetWord8 off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds


instance Dimensions ds => ElementWise (Idx ds) Word (ArrayT Word ds) where
    indexOffset# x i = js_indexArrayOffsetWord i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetWord j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewWordArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (W# v) = case fromEnum i of I# j -> js_setArrayOffsetWord# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createWordArray (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetWord off x) of
                      (W# r) -> js_writeArrayOffsetWord# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createWordArray n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (W# r) -> js_writeArrayOffsetWord# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createWordArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W# z) u a s -> js_writeArrayOffsetWord# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetWord off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetWord off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createWordArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(W# z) u a s -> js_writeArrayOffsetWord# off z a (u a s) ) <$> f i (js_indexArrayOffsetWord off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createWordArray n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(W# z) u a s -> js_writeArrayOffsetWord# off z a (u a s) ) <$> f (js_indexArrayOffsetWord off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds




instance Dimensions ds => ElementWise (Idx ds) Word8Clamped (ArrayT Word8Clamped ds) where
    indexOffset# x i = js_indexArrayOffsetWord8Clamped i x
    {-# INLINE indexOffset# #-}
    x ! i = case fromEnum i of I# j -> js_indexArrayOffsetWord8Clamped j x
    {-# INLINE (!) #-}
    broadcast = js_fillNewWord8ClampedArray (totalDim (dim @ds))
    {-# INLINE broadcast #-}
    update i (Clamped (I# v)) = case fromEnum i of I# j -> js_setArrayOffsetWord8Clamped# j v
    {-# INLINE update #-}

    ewmap f x = case runRW#
         (\s0 -> case js_createWord8ClampedArray (js_length x) s0 of
           (# s1, my #) -> case overDim_# (dim `inSpaceOf` x)
                   ( \ii off -> case f ii (js_indexArrayOffsetWord8Clamped off x) of
                      (Clamped (I# r)) -> js_writeArrayOffsetWord8Clamped# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
    {-# INLINE ewmap #-}

    ewgen f = case runRW#
         (\s0 -> case js_createWord8ClampedArray n s0 of
           (# s1, my #) -> case overDim_# dds
                   ( \ii off -> case f ii of
                      (Clamped (I# r)) -> js_writeArrayOffsetWord8Clamped# off r my
                   ) 0# 1# s1 of
               s3 -> unsafeFreezeArrayT# my s3
         ) of (# _, r #) -> r
        where
          dds = dim @ds
          n = case totalDim dds of I# d -> d
    {-# INLINE ewgen #-}

    ewgenA f = wr (js_createWord8ClampedArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(Clamped (I# z)) u a s -> js_writeArrayOffsetWord8Clamped# off z a (u a s) ) <$> f i <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    ewfoldr f v0 x
        = foldDimReverse (dim `inSpaceOf` x)
          (\ii off a -> f ii (js_indexArrayOffsetWord8Clamped off x) a) 0# 1# v0
    {-# INLINE ewfoldr #-}

    ewfoldl f v0 x
        = foldDim (dim `inSpaceOf` x)
          (\ii off a -> f ii a (js_indexArrayOffsetWord8Clamped off x)) 0# 1# v0
    {-# INLINE ewfoldl #-}

    indexWise f x = wr (js_createWord8ClampedArray n) <$> foldDim dds g 0# 1# (pure (\_ s -> s))
        where
          g i off mf = (\(Clamped (I# z)) u a s -> js_writeArrayOffsetWord8Clamped# off z a (u a s) ) <$> f i (js_indexArrayOffsetWord8Clamped off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds

    elementWise f x = wr (js_createWord8ClampedArray n) <$> foldDimOff dds g 0# 1# (pure (\_ s -> s))
        where
          g off mf = (\(Clamped (I# z)) u a s -> js_writeArrayOffsetWord8Clamped# off z a (u a s) ) <$> f (js_indexArrayOffsetWord8Clamped off x) <*> mf
          n = case totalDim dds of I# d -> d
          dds = dim @ds







instance (KnownDim n, KnownDim m, ArrayT t '[n,m] ~ Array t '[n,m], 2 <= n, 2 <= m)
      => MatrixCalculus t n m where
    transpose = KnownDataFrame . js_transpose @t @n @m (dimVal' @n) . _getDF

foreign import javascript unsafe "h$easytensor_transpose($1, $2)" js_transpose :: Int -> ArrayT t '[n,m] -> ArrayT t '[m,n]


instance ( KnownDim n, ArrayT Float '[n,n] ~ Array Float '[n,n] )
      => SquareMatrixCalculus Float n where
    eye = KnownDataFrame $ js_eyeFloat (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagFloat (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceFloat m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detFloat m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Double '[n,n] ~ Array Double '[n,n] )
      => SquareMatrixCalculus Double n where
    eye = KnownDataFrame $ js_eyeDouble (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagDouble (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceDouble m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detDouble m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Int '[n,n] ~ Array Int '[n,n] )
      => SquareMatrixCalculus Int n where
    eye = KnownDataFrame $ js_eyeInt (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagInt (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceInt m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detInt m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Int8 '[n,n] ~ Array Int8 '[n,n] )
      => SquareMatrixCalculus Int8 n where
    eye = KnownDataFrame $ js_eyeInt8 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagInt8 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceInt8 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detInt8 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Int16 '[n,n] ~ Array Int16 '[n,n] )
      => SquareMatrixCalculus Int16 n where
    eye = KnownDataFrame $ js_eyeInt16 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagInt16 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceInt16 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detInt16 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Int32 '[n,n] ~ Array Int32 '[n,n] )
      => SquareMatrixCalculus Int32 n where
    eye = KnownDataFrame $ js_eyeInt32 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagInt32 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceInt32 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detInt32 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Word '[n,n] ~ Array Word '[n,n] )
      => SquareMatrixCalculus Word n where
    eye = KnownDataFrame $ js_eyeWord (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagWord (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceWord m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detWord m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Word8 '[n,n] ~ Array Word8 '[n,n] )
      => SquareMatrixCalculus Word8 n where
    eye = KnownDataFrame $ js_eyeWord8 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagWord8 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceWord8 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detWord8 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Word16 '[n,n] ~ Array Word16 '[n,n] )
      => SquareMatrixCalculus Word16 n where
    eye = KnownDataFrame $ js_eyeWord16 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagWord16 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceWord16 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detWord16 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Word32 '[n,n] ~ Array Word32 '[n,n] )
      => SquareMatrixCalculus Word32 n where
    eye = KnownDataFrame $ js_eyeWord32 (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagWord32 (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceWord32 m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detWord32 m (dimVal' @n)
    {-# INLINE det #-}

instance ( KnownDim n, ArrayT Word8Clamped '[n,n] ~ Array Word8Clamped '[n,n] )
      => SquareMatrixCalculus Word8Clamped n where
    eye = KnownDataFrame $ js_eyeWord8Clamped (dimVal' @n)
    {-# INLINE eye #-}
    diag (KnownDataFrame (Scalar x)) = KnownDataFrame $ js_diagWord8Clamped (dimVal' @n) x
    {-# INLINE diag #-}
    trace (KnownDataFrame m) = KnownDataFrame . Scalar $ js_traceWord8Clamped m (dimVal' @n)
    {-# INLINE trace #-}
    det (KnownDataFrame m) = KnownDataFrame . Scalar $ js_detWord8Clamped m (dimVal' @n)
    {-# INLINE det #-}

foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detFloat        :: ArrayT Float        '[n,n] -> Int -> Float
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detDouble       :: ArrayT Double       '[n,n] -> Int -> Double
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detInt          :: ArrayT Int          '[n,n] -> Int -> Int
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detInt8         :: ArrayT Int8         '[n,n] -> Int -> Int8
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detInt16        :: ArrayT Int16        '[n,n] -> Int -> Int16
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detInt32        :: ArrayT Int32        '[n,n] -> Int -> Int32
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detWord         :: ArrayT Word         '[n,n] -> Int -> Word
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detWord8        :: ArrayT Word8        '[n,n] -> Int -> Word8
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detWord16       :: ArrayT Word16       '[n,n] -> Int -> Word16
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detWord32       :: ArrayT Word32       '[n,n] -> Int -> Word32
foreign import javascript unsafe "h$easytensor_det($1, $2)" js_detWord8Clamped :: ArrayT Word8Clamped '[n,n] -> Int -> Word8Clamped

foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceFloat        :: ArrayT Float        '[n,n] -> Int -> Float
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceDouble       :: ArrayT Double       '[n,n] -> Int -> Double
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceInt          :: ArrayT Int          '[n,n] -> Int -> Int
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceInt8         :: ArrayT Int8         '[n,n] -> Int -> Int8
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceInt16        :: ArrayT Int16        '[n,n] -> Int -> Int16
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceInt32        :: ArrayT Int32        '[n,n] -> Int -> Int32
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceWord         :: ArrayT Word         '[n,n] -> Int -> Word
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceWord8        :: ArrayT Word8        '[n,n] -> Int -> Word8
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceWord16       :: ArrayT Word16       '[n,n] -> Int -> Word16
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceWord32       :: ArrayT Word32       '[n,n] -> Int -> Word32
foreign import javascript unsafe "h$easytensor_trace($1, $2)" js_traceWord8Clamped :: ArrayT Word8Clamped '[n,n] -> Int -> Word8Clamped


foreign import javascript unsafe "h$easytensor_diagFloat32($1, $2)" js_diagFloat        :: Int -> Float  -> ArrayT Float        '[n,n]
foreign import javascript unsafe "h$easytensor_diagFloat64($1, $2)" js_diagDouble       :: Int -> Double -> ArrayT Double       '[n,n]
foreign import javascript unsafe "h$easytensor_diagInt32($1, $2)"   js_diagInt          :: Int -> Int    -> ArrayT Int          '[n,n]
foreign import javascript unsafe "h$easytensor_diagInt8($1, $2)"    js_diagInt8         :: Int -> Int8   -> ArrayT Int8         '[n,n]
foreign import javascript unsafe "h$easytensor_diagInt16($1, $2)"   js_diagInt16        :: Int -> Int16  -> ArrayT Int16        '[n,n]
foreign import javascript unsafe "h$easytensor_diagInt32($1, $2)"   js_diagInt32        :: Int -> Int32  -> ArrayT Int32        '[n,n]
foreign import javascript unsafe "h$easytensor_diagUint($1, $2)"    js_diagWord         :: Int -> Word   -> ArrayT Word         '[n,n]
foreign import javascript unsafe "h$easytensor_diagUint8($1, $2)"   js_diagWord8        :: Int -> Word8  -> ArrayT Word8        '[n,n]
foreign import javascript unsafe "h$easytensor_diagUint16($1, $2)"  js_diagWord16       :: Int -> Word16 -> ArrayT Word16       '[n,n]
foreign import javascript unsafe "h$easytensor_diagUint32($1, $2)"  js_diagWord32       :: Int -> Word32 -> ArrayT Word32       '[n,n]
foreign import javascript unsafe "h$easytensor_diagUint8Clamped($1, $2)" js_diagWord8Clamped :: Int -> Word8Clamped ->ArrayT Word8Clamped '[n,n]


foreign import javascript unsafe "h$easytensor_eyeFloat32($1)" js_eyeFloat        :: Int -> ArrayT Float        '[n,n]
foreign import javascript unsafe "h$easytensor_eyeFloat64($1)" js_eyeDouble       :: Int -> ArrayT Double       '[n,n]
foreign import javascript unsafe "h$easytensor_eyeInt32($1)"   js_eyeInt          :: Int -> ArrayT Int          '[n,n]
foreign import javascript unsafe "h$easytensor_eyeInt8($1)"    js_eyeInt8         :: Int -> ArrayT Int8         '[n,n]
foreign import javascript unsafe "h$easytensor_eyeInt16($1)"   js_eyeInt16        :: Int -> ArrayT Int16        '[n,n]
foreign import javascript unsafe "h$easytensor_eyeInt32($1)"   js_eyeInt32        :: Int -> ArrayT Int32        '[n,n]
foreign import javascript unsafe "h$easytensor_eyeUint($1)"    js_eyeWord         :: Int -> ArrayT Word         '[n,n]
foreign import javascript unsafe "h$easytensor_eyeUint8($1)"   js_eyeWord8        :: Int -> ArrayT Word8        '[n,n]
foreign import javascript unsafe "h$easytensor_eyeUint16($1)"  js_eyeWord16       :: Int -> ArrayT Word16       '[n,n]
foreign import javascript unsafe "h$easytensor_eyeUint32($1)"  js_eyeWord32       :: Int -> ArrayT Word32       '[n,n]
foreign import javascript unsafe "h$easytensor_eyeUint8Clamped($1)" js_eyeWord8Clamped :: Int -> ArrayT Word8Clamped '[n,n]




instance (Fractional t, KnownNat n, ArrayT t '[n,n] ~ Array t '[n,n], 2 <= n) => MatrixInverse t n where
    inverse (KnownDataFrame m) = KnownDataFrame $ js_inverse m (dimVal' @n)

foreign import javascript unsafe "h$easytensor_inverse($1, $2)"   js_inverse :: ArrayT t '[n,n] -> Int -> ArrayT t '[n,n]














unsafeFreezeArrayT# :: MutableArrayT s t ds -> State# s -> (# State# s, ArrayT t ds #)
unsafeFreezeArrayT# a s = (# s, coerce a #)
{-# INLINE unsafeFreezeArrayT# #-}

--unsafeThawArrayT# :: ArrayT t ds -> State# s -> (#State# s, MutableArrayT s t ds #)
--unsafeThawArrayT# a s = (# s, coerce a #)
--{-# INLINE unsafeThawArrayT# #-}


foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetFloat#        :: Int# -> ArrayT Float        ds -> Float#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetDouble#       :: Int# -> ArrayT Double       ds -> Double#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt#          :: Int# -> ArrayT Int          ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt8#         :: Int# -> ArrayT Int8         ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt16#        :: Int# -> ArrayT Int16        ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt32#        :: Int# -> ArrayT Int32        ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord#         :: Int# -> ArrayT Word         ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8#        :: Int# -> ArrayT Word8        ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8Clamped# :: Int# -> ArrayT Word8Clamped ds -> Int#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord16#       :: Int# -> ArrayT Word16       ds -> Word#
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord32#       :: Int# -> ArrayT Word32       ds -> Word#


foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetFloat        :: Int# -> ArrayT Float        ds -> Float
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetDouble       :: Int# -> ArrayT Double       ds -> Double
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt          :: Int# -> ArrayT Int          ds -> Int
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt8         :: Int# -> ArrayT Int8         ds -> Int8
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt16        :: Int# -> ArrayT Int16        ds -> Int16
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetInt32        :: Int# -> ArrayT Int32        ds -> Int32
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord         :: Int# -> ArrayT Word         ds -> Word
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8        :: Int# -> ArrayT Word8        ds -> Word8
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord8Clamped :: Int# -> ArrayT Word8Clamped ds -> Word8Clamped
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord16       :: Int# -> ArrayT Word16       ds -> Word16
foreign import javascript unsafe "$2[$1]" js_indexArrayOffsetWord32       :: Int# -> ArrayT Word32       ds -> Word32


foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetFloat#        :: Int# -> Float#  -> ArrayT Float        ds -> ArrayT Float ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetDouble#       :: Int# -> Double# -> ArrayT Double       ds -> ArrayT Double ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetInt#          :: Int# -> Int#    -> ArrayT Int          ds -> ArrayT Int ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetInt8#         :: Int# -> Int#    -> ArrayT Int8         ds -> ArrayT Int8 ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetInt16#        :: Int# -> Int#    -> ArrayT Int16        ds -> ArrayT Int16 ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetInt32#        :: Int# -> Int#    -> ArrayT Int32        ds -> ArrayT Int32 ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetWord#         :: Int# -> Word#   -> ArrayT Word         ds -> ArrayT Word ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetWord8#        :: Int# -> Word#   -> ArrayT Word8        ds -> ArrayT Word8 ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetWord8Clamped# :: Int# -> Int#    -> ArrayT Word8Clamped ds -> ArrayT Word8Clamped ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetWord16#       :: Int# -> Word#   -> ArrayT Word16       ds -> ArrayT Word16 ds
foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;" js_setArrayOffsetWord32#       :: Int# -> Word#   -> ArrayT Word32       ds -> ArrayT Word32 ds




--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetFloat#        :: Int# -> MutableArrayT s Float        ds -> State# s -> (# State# s, Float# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetDouble#       :: Int# -> MutableArrayT s Double       ds -> State# s -> (# State# s, Double# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt#          :: Int# -> MutableArrayT s Int          ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt8#         :: Int# -> MutableArrayT s Int8         ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt16#        :: Int# -> MutableArrayT s Int16        ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetInt32#        :: Int# -> MutableArrayT s Int32        ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord#         :: Int# -> MutableArrayT s Word         ds -> State# s -> (# State# s, Word# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord8#        :: Int# -> MutableArrayT s Word8        ds -> State# s -> (# State# s, Word# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord8Clamped# :: Int# -> MutableArrayT s Word8Clamped ds -> State# s -> (# State# s, Int#  #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord16#       :: Int# -> MutableArrayT s Word16       ds -> State# s -> (# State# s, Word# #)
--foreign import javascript unsafe "$2[$1]" js_readArrayOffsetWord32#       :: Int# -> MutableArrayT s Word32       ds -> State# s -> (# State# s, Word# #)


foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetFloat#        :: Int# -> Float#  -> MutableArrayT s Float        ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetDouble#       :: Int# -> Double# -> MutableArrayT s Double       ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt#          :: Int# -> Int#    -> MutableArrayT s Int          ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt8#         :: Int# -> Int#    -> MutableArrayT s Int8         ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt16#        :: Int# -> Int#    -> MutableArrayT s Int16        ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetInt32#        :: Int# -> Int#    -> MutableArrayT s Int32        ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord#         :: Int# -> Word#   -> MutableArrayT s Word         ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord8#        :: Int# -> Word#   -> MutableArrayT s Word8        ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord8Clamped# :: Int# -> Int#    -> MutableArrayT s Word8Clamped ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord16#       :: Int# -> Word#   -> MutableArrayT s Word16       ds -> State# s -> State# s
foreign import javascript unsafe "$3[$1] = $2;" js_writeArrayOffsetWord32#       :: Int# -> Word#   -> MutableArrayT s Word32       ds -> State# s -> State# s








-----------------------------------------------------------------------------
-- Conversions between types
-----------------------------------------------------------------------------




foreign import javascript unsafe "$1.length"     js_length     :: ArrayT t ds -> Int#
foreign import javascript unsafe "$1.byteOffset" js_byteOffset :: ArrayT t ds -> Int#
--foreign import javascript unsafe "$1.byteLength" js_byteLength :: ArrayT t ds -> Int#


--foreign import javascript unsafe "$1.length"     js_lengthM     :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$1.byteOffset" js_byteOffsetM :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)
--foreign import javascript unsafe "$1.byteLength" js_byteLengthM :: MutableArrayT s t ds -> State# s -> (# State# s, Int# #)

foreign import javascript unsafe "h$wrapBuffer($1.buffer)" js_wrapArrayT        :: ArrayT t ds -> ByteArray#
--foreign import javascript unsafe "h$wrapBuffer($1.buffer)" js_wrapMutableArrayT :: MutableArrayT s t ds -> State# s -> (# State# s, MutableByteArray# s #)
--
--
--foreign import javascript unsafe "$1.f3 || new Float32Array($1.buf)"      js_unwrapFloatArray        :: ByteArray# -> ArrayT Float ds
--foreign import javascript unsafe "$1.f6 || new Float64Array($1.buf)"      js_unwrapDoubleArray       :: ByteArray# -> ArrayT Double ds
--foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapIntArray          :: ByteArray# -> ArrayT Int ds
--foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapInt32Array        :: ByteArray# -> ArrayT Int32 ds
--foreign import javascript unsafe "$1.i1 || new Int16Array($1.buf)"        js_unwrapInt16Array        :: ByteArray# -> ArrayT Int16 ds
--foreign import javascript unsafe "$1.i8 || new Int8Array($1.buf)"         js_unwrapInt8Array         :: ByteArray# -> ArrayT Int8 ds
--foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapWordArray         :: ByteArray# -> ArrayT Word ds
--foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapWord32Array       :: ByteArray# -> ArrayT Word32 ds
--foreign import javascript unsafe "$1.u1 || new Uint16Array($1.buf)"       js_unwrapWord16Array       :: ByteArray# -> ArrayT Word16 ds
--foreign import javascript unsafe "$1.u8 || new Uint8Array($1.buf)"        js_unwrapWord8Array        :: ByteArray# -> ArrayT Word8 ds
--foreign import javascript unsafe "$1.uc || new Uint8ClampedArray($1.buf)" js_unwrapWord8ClampedArray :: ByteArray# -> ArrayT Word8Clamped ds



foreign import javascript unsafe "new Float32Array($1.buf, $2*4, $3)"    js_unwrapFloatArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Float ds
foreign import javascript unsafe "new Float64Array($1.buf, $2*8, $3)"    js_unwrapDoubleArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Double ds
foreign import javascript unsafe "new Int32Array($1.buf, $2*4, $3)"      js_unwrapIntArrayOffLen          :: ByteArray# -> Int# -> Int# -> ArrayT Int ds
foreign import javascript unsafe "new Int32Array($1.buf, $2*4, $3)"      js_unwrapInt32ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Int32 ds
foreign import javascript unsafe "new Int16Array($1.buf, $2*2, $3)"      js_unwrapInt16ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Int16 ds
foreign import javascript unsafe "new Int8Array($1.buf, $2, $3)"         js_unwrapInt8ArrayOffLen         :: ByteArray# -> Int# -> Int# -> ArrayT Int8 ds
foreign import javascript unsafe "new Uint32Array($1.buf, $2*4, $3)"     js_unwrapWordArrayOffLen         :: ByteArray# -> Int# -> Int# -> ArrayT Word ds
foreign import javascript unsafe "new Uint32Array($1.buf, $2*4, $3)"     js_unwrapWord32ArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Word32 ds
foreign import javascript unsafe "new Uint16Array($1.buf, $2*2, $3)"     js_unwrapWord16ArrayOffLen       :: ByteArray# -> Int# -> Int# -> ArrayT Word16 ds
foreign import javascript unsafe "new Uint8Array($1.buf, $2, $3)"        js_unwrapWord8ArrayOffLen        :: ByteArray# -> Int# -> Int# -> ArrayT Word8 ds
foreign import javascript unsafe "new Uint8ClampedArray($1.buf, $2, $3)" js_unwrapWord8ClampedArrayOffLen :: ByteArray# -> Int# -> Int# -> ArrayT Word8Clamped ds


--foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapMutableIntArray          :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int ds #)
--foreign import javascript unsafe "$1.i3 || new Int32Array($1.buf)"        js_unwrapMutableInt32Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int32 ds #)
--foreign import javascript unsafe "$1.i1 || new Int16Array($1.buf)"        js_unwrapMutableInt16Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int16 ds #)
--foreign import javascript unsafe "$1.i8 || new Int8Array($1.buf)"         js_unwrapMutableInt8Array         :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Int8 ds #)
--foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapMutableWordArray         :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word ds #)
--foreign import javascript unsafe "$1.u3 || new Uint32Array($1.buf)"       js_unwrapMutableWord32Array       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word32 ds #)
--foreign import javascript unsafe "$1.u1 || new Uint16Array($1.buf)"       js_unwrapMutableWord16Array       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word16 ds #)
--foreign import javascript unsafe "$1.u8 || new Uint8Array($1.buf)"        js_unwrapMutableWord8Array        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word8 ds #)
--foreign import javascript unsafe "$1.f3 || new Float32Array($1.buf)"      js_unwrapMutableFloatArray        :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Float ds #)
--foreign import javascript unsafe "$1.f6 || new Float64Array($1.buf)"      js_unwrapMutableDoubleArray       :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Double ds #)
--foreign import javascript unsafe "$1.uc || new Uint8ClampedArray($1.buf)" js_unwrapMutableWord8ClampedArray :: MutableByteArray# s -> State# s -> (# State# s, MutableArrayT s Word8Clamped ds #)



-----------------------------------------------------------------------------
-- Create new arrays
-----------------------------------------------------------------------------

foreign import javascript unsafe "new Float32Array($1)"      js_createFloatArray        :: Int# -> State# s -> (# State# s, MutableArrayT s Float ds #)
foreign import javascript unsafe "new Float64Array($1)"      js_createDoubleArray       :: Int# -> State# s -> (# State# s, MutableArrayT s Double ds #)
foreign import javascript unsafe "new Int32Array($1)"        js_createIntArray          :: Int# -> State# s -> (# State# s, MutableArrayT s Int ds #)
foreign import javascript unsafe "new Int32Array($1)"        js_createInt32Array        :: Int# -> State# s -> (# State# s, MutableArrayT s Int32 ds #)
foreign import javascript unsafe "new Int16Array($1)"        js_createInt16Array        :: Int# -> State# s -> (# State# s, MutableArrayT s Int16 ds #)
foreign import javascript unsafe "new Int8Array($1)"         js_createInt8Array         :: Int# -> State# s -> (# State# s, MutableArrayT s Int8 ds #)
foreign import javascript unsafe "new Uint32Array($1)"       js_createWordArray         :: Int# -> State# s -> (# State# s, MutableArrayT s Word ds #)
foreign import javascript unsafe "new Uint32Array($1)"       js_createWord32Array       :: Int# -> State# s -> (# State# s, MutableArrayT s Word32 ds #)
foreign import javascript unsafe "new Uint16Array($1)"       js_createWord16Array       :: Int# -> State# s -> (# State# s, MutableArrayT s Word16 ds #)
foreign import javascript unsafe "new Uint8Array($1)"        js_createWord8Array        :: Int# -> State# s -> (# State# s, MutableArrayT s Word8 ds #)
foreign import javascript unsafe "new Uint8ClampedArray($1)" js_createWord8ClampedArray :: Int# -> State# s -> (# State# s, MutableArrayT s Word8Clamped ds #)

foreign import javascript unsafe "new Float32Array($1).fill($2)"      js_fillNewFloatArray        :: Int -> Float        -> ArrayT Float ds
foreign import javascript unsafe "new Float64Array($1).fill($2)"      js_fillNewDoubleArray       :: Int -> Double       -> ArrayT Double ds
foreign import javascript unsafe "new Int32Array($1).fill($2)"        js_fillNewIntArray          :: Int -> Int          -> ArrayT Int ds
foreign import javascript unsafe "new Int32Array($1).fill($2)"        js_fillNewInt32Array        :: Int -> Int32        -> ArrayT Int32 ds
foreign import javascript unsafe "new Int16Array($1).fill($2)"        js_fillNewInt16Array        :: Int -> Int16        -> ArrayT Int16 ds
foreign import javascript unsafe "new Int8Array($1).fill($2)"         js_fillNewInt8Array         :: Int -> Int8         -> ArrayT Int8 ds
foreign import javascript unsafe "new Uint32Array($1).fill($2)"       js_fillNewWordArray         :: Int -> Word         -> ArrayT Word ds
foreign import javascript unsafe "new Uint32Array($1).fill($2)"       js_fillNewWord32Array       :: Int -> Word32       -> ArrayT Word32 ds
foreign import javascript unsafe "new Uint16Array($1).fill($2)"       js_fillNewWord16Array       :: Int -> Word16       -> ArrayT Word16 ds
foreign import javascript unsafe "new Uint8Array($1).fill($2)"        js_fillNewWord8Array        :: Int -> Word8        -> ArrayT Word8 ds
foreign import javascript unsafe "new Uint8ClampedArray($1).fill($2)" js_fillNewWord8ClampedArray :: Int -> Word8Clamped -> ArrayT Word8Clamped ds





-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Float32Array(arr.length); $r.set(arr);"      js_fromListFloatArray        :: Exts.Any -> ArrayT Float ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Float64Array(arr.length); $r.set(arr);"      js_fromListDoubleArray       :: Exts.Any -> ArrayT Double ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int32Array(arr.length); $r.set(arr);"        js_fromListIntArray          :: Exts.Any -> ArrayT Int ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int32Array(arr.length); $r.set(arr);"        js_fromListInt32Array        :: Exts.Any -> ArrayT Int32 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int16Array(arr.length); $r.set(arr);"        js_fromListInt16Array        :: Exts.Any -> ArrayT Int16 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Int8Array(arr.length); $r.set(arr);"         js_fromListInt8Array         :: Exts.Any -> ArrayT Int8 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint32Array(arr.length); $r.set(arr);"       js_fromListWordArray         :: Exts.Any -> ArrayT Word ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint32Array(arr.length); $r.set(arr);"       js_fromListWord32Array       :: Exts.Any -> ArrayT Word32 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint16Array(arr.length); $r.set(arr);"       js_fromListWord16Array       :: Exts.Any -> ArrayT Word16 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint8Array(arr.length); $r.set(arr);"        js_fromListWord8Array        :: Exts.Any -> ArrayT Word8 ds
-- foreign import javascript unsafe "var arr = LikeHS.listToArrayNoUnwrap($1); $r = new Uint8ClampedArray(arr.length); $r.set(arr);" js_fromListWord8ClampedArray :: Exts.Any -> ArrayT Word8Clamped ds


-- foreign import javascript unsafe "$r = new Float32Array($1.length); $r.set($1);" js_fromArrayFloatArray        :: SomeTypedArray m0 t -> ArrayT Float ds
-- foreign import javascript unsafe "new Float32Array($1)" js_viewFloatArray        :: SomeArrayBuffer m -> ArrayT Float ds
--
-- foreign import javascript unsafe "$r = new Float64Array($1.length); $r.set($1);" js_fromArrayDoubleArray       :: SomeTypedArray m0 t -> ArrayT Double ds
-- foreign import javascript unsafe "new Float64Array($1)" js_viewDoubleArray       :: SomeArrayBuffer m -> ArrayT Double ds
--
-- foreign import javascript unsafe "$r = new Int32Array($1.length); $r.set($1);" js_fromArrayIntArray          :: SomeTypedArray m0 t -> ArrayT Int ds
-- foreign import javascript unsafe "new Int32Array($1)" js_viewIntArray          :: SomeArrayBuffer m -> ArrayT Int ds
--
-- foreign import javascript unsafe "$r = new Int32Array($1.length); $r.set($1);" js_fromArrayInt32Array :: SomeTypedArray m0 t -> ArrayT Int32 ds
-- foreign import javascript unsafe "new Int32Array($1)" js_viewInt32Array :: SomeArrayBuffer m -> ArrayT Int32 ds
--
-- foreign import javascript unsafe "$r = new Int16Array($1.length); $r.set($1);" js_fromArrayInt16Array :: SomeTypedArray m0 t -> ArrayT Int16 ds
-- foreign import javascript unsafe "new Int16Array($1)" js_viewInt16Array :: SomeArrayBuffer m -> ArrayT Int16 ds
--
-- foreign import javascript unsafe "$r = new Int8Array($1.length); $r.set($1);" js_fromArrayInt8Array :: SomeTypedArray m0 t -> ArrayT Int8 ds
-- foreign import javascript unsafe "new Int8Array($1)" js_viewInt8Array :: SomeArrayBuffer m -> ArrayT Int8 ds
--
-- foreign import javascript unsafe "$r = new Uint32Array($1.length); $r.set($1);" js_fromArrayWordArray :: SomeTypedArray m0 t -> ArrayT Word ds
-- foreign import javascript unsafe "new Uint32Array($1)" js_viewWordArray :: SomeArrayBuffer m -> ArrayT Word ds
--
-- foreign import javascript unsafe "$r = new Uint32Array($1.length); $r.set($1);" js_fromArrayWord32Array :: SomeTypedArray m0 t -> ArrayT Word32 ds
-- foreign import javascript unsafe "new Uint32Array($1)" js_viewWord32Array :: SomeArrayBuffer m -> ArrayT Word32 ds
--
-- foreign import javascript unsafe "$r = new Uint16Array($1.length); $r.set($1);" js_fromArrayWord16Array :: SomeTypedArray m0 t -> ArrayT Word16 ds
-- foreign import javascript unsafe "new Uint16Array($1)" js_viewWord16Array :: SomeArrayBuffer m -> ArrayT Word16 ds
--
-- foreign import javascript unsafe "$r = new Uint8Array($1.length); $r.set($1);" js_fromArrayWord8Array        :: SomeTypedArray m0 t -> ArrayT Word8 ds
-- foreign import javascript unsafe "new Uint8Array($1)" js_viewWord8Array        :: SomeArrayBuffer m -> ArrayT Word8 ds
--
-- foreign import javascript unsafe "$r = new Uint8ClampedArray($1.length); $r.set($1);" js_fromArrayWord8ClampedArray :: SomeTypedArray m0 t -> ArrayT Word8Clamped ds
-- foreign import javascript unsafe "new Uint8ClampedArray($1)" js_viewWord8ClampedArray :: SomeArrayBuffer m -> ArrayT Word8Clamped ds
