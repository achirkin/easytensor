{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module OpenCV.Core.Types.Mat.DataFrame
    ( SChMatDF (..)
    , MChMatDF (..)
    , DataFrameOpenCVMat (..)
    , XNatToDS, DSToXNat, DSToXNatKind
    ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import OpenCV.Internal.C.Inline ( openCvCtx )
import OpenCV.Internal.C.Types (fromPtr)
import OpenCV.Internal.Core.Types.Mat (withMatData, Mat (..), unmarshalFlags)
import OpenCV.Core.Types.Mat
import OpenCV.TypeLevel
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)
import Data.Int

import Numeric.DataFrame
import Numeric.Dimensions
import Numeric.Dimensions.XDim
import Numeric.TypeLits

C.context openCvCtx

#include "MachDeps.h"
C.include "opencv2/core.hpp"
C.using "namespace cv"

-- | DataFrame as a Single Channel Matrix
newtype SChMatDF t (ds :: [k])
  = SChMatDF { unSChMatDF :: DataFrame t ds }

-- | DataFrame as a Single Channel Matrix
newtype MChMatDF t (d :: k) (ds :: [k])
  = MChMatDF { unMChMatDF :: DataFrame t (d ': ds) }


type family XNatToDS (xs :: [XNat]) :: [DS Nat] where
  XNatToDS '[] = '[]
  XNatToDS ('N n ': xs) = 'S n ': XNatToDS xs
  XNatToDS ('XN _ ': xs) = 'OpenCV.TypeLevel.D ': XNatToDS xs


type family DSToXNat (xs :: [DS Nat]) :: [k] where
  DSToXNat '[] = ('[] :: [Nat])
  DSToXNat ('S 1 ': xs) = DSToXNat xs
  DSToXNat ('S n ': xs) = ConsDim n (DSToXNat xs)
  DSToXNat ('OpenCV.TypeLevel.D ': xs) = ConsDim ('XN 1) (DSToXNat xs)

type family DSToXNatKind (xs :: [DS Nat]) :: k where
  DSToXNatKind '[] = Nat
  DSToXNatKind ('S _ ': xs) = DSToXNatKind xs
  DSToXNatKind ('OpenCV.TypeLevel.D ': _) = XNat


type instance MatShape (SChMatDF t (ds :: [Nat])) = 'S (DSNats ds)
type instance MatShape (SChMatDF t (ds :: [XNat])) = 'S (XNatToDS ds)
type instance MatShape (MChMatDF t d (ds :: [Nat])) = 'S (DSNats ds)
type instance MatShape (MChMatDF t d (ds :: [XNat])) = 'S (XNatToDS ds)

type instance MatChannels (SChMatDF t ds) = 'S 1
type instance MatChannels (MChMatDF t c ds) = 'S c

type instance MatDepth (SChMatDF t ds)   = 'S t
type instance MatDepth (MChMatDF t d ds) = 'S t


openCVType :: ElemType t -> C.CInt -> IO C.CInt
openCVType ETFloat  channels = [CU.exp| int { CV_32FC($(int channels)) } |]
openCVType ETDouble channels = [CU.exp| int { CV_64FC($(int channels)) } |]
#if WORD_SIZE_IN_BITS < 64
openCVType ETInt    channels = [CU.exp| int { CV_32SC($(int channels)) } |]
#else
openCVType ETInt    _        = return 0
#endif
openCVType ETInt8   channels = [CU.exp| int { CV_8SC($(int channels)) } |]
openCVType ETInt16  channels = [CU.exp| int { CV_16SC($(int channels)) } |]
openCVType ETInt32  channels = [CU.exp| int { CV_32SC($(int channels)) } |]
openCVType ETInt64  _        = return 0
openCVType ETWord   _        = return 0
openCVType ETWord8  channels = [CU.exp| int { CV_8UC($(int channels)) } |]
openCVType ETWord16 channels = [CU.exp| int { CV_16UC($(int channels)) } |]
openCVType ETWord32 _        = return 0
openCVType ETWord64 _        = return 0


instance NumericFrame t ds
       => ToMat (SChMatDF t (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (SChMatDF df) = unsafePerformIO . fromPtr
                             . allocaArray dimN $ \sizesPtr ->
                               alloca $ \dataPtr -> do
      poke dataPtr df
      writeDim sizesPtr 0 (dim @ds)
      cvType <- openCVType (elemTypeInstance @t) 1
      let ptr' = unsafeCoerce dataPtr :: Ptr ()
      [CU.exp| Mat * { new cv::Mat( $(int cdimN), $(int * sizesPtr), $(int cvType), $(void * ptr')) } |]
    where
      dimN = order @Nat @ds
      cdimN = fromIntegral dimN :: C.CInt

instance ToMat (SChMatDF t (xds :: [XNat])) where
  toMat (SChMatDF (SomeDataFrame (df :: DataFrame t ds))) = unsafeCoerce . toMat $ SChMatDF df


instance NumericFrame t (d ': ds)
       => ToMat (MChMatDF t d (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (MChMatDF df)
    | Just Evidence <- inferUnConsDimensions @(d ': ds)
    = unsafePerformIO . fromPtr
                             . allocaArray dimN $ \sizesPtr ->
                               alloca $ \dataPtr -> do
      poke dataPtr df
      writeDim sizesPtr 0 (dim @ds)
      cvType <- openCVType (elemTypeInstance @t) (fromIntegral $ dimVal' @d )
      let ptr' = unsafeCoerce dataPtr :: Ptr ()
      [CU.exp| Mat * { new cv::Mat( $(int cdimN), $(int * sizesPtr), $(int cvType), $(void * ptr')) } |]
    where
      dimN = order @Nat @(d ': ds) - 1
      cdimN = fromIntegral dimN :: C.CInt
  toMat _ = error "Impossible happened: toMat was called on scalar as a multichannel matrix"

instance ToMat (MChMatDF t xd (xds :: [XNat])) where
  toMat (MChMatDF (SomeDataFrame (df :: DataFrame t ds)))
    | (Evidence :: Evidence (ds ~ (n':ns))) <- unsafeCoerce (Evidence :: Evidence (ds ~ ds))
    = unsafeCoerce . toMat $ MChMatDF df


writeDim :: forall (xs :: [k]) . Ptr C.CInt -> Int -> Dim xs -> IO ()
writeDim _ _ Numeric.Dimensions.D = return ()
writeDim p i (d :* ds) = pokeElemOff p i (fromIntegral $ dimVal d) >> writeDim p (i+1) ds

instance Storable (DataFrame t ds)
      => FromMat (SChMatDF t (ds :: [Nat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO . withMatData m $ \_ -> fmap SChMatDF . peek . unsafeCoerce

instance Storable (DataFrame t (d ': ds))
      => FromMat (MChMatDF t d (ds :: [Nat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO . withMatData m $ \_ -> fmap MChMatDF . peek . unsafeCoerce


instance ( XDimensions xds, ElemTypeInference t)
      => FromMat (SChMatDF t (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO . withMatData m $ \dims ptr ->
      case someDimsVal $ map fromIntegral dims of
        Nothing -> error "fromMat: Could not create runtime-known Dim."
        Just (SomeDims ds) -> case xDimVal <$> wrapDim @xds ds of
          Nothing -> error "fromMat: Mat dimensions do not agree with the specified XDim"
          Just (XDim (dds :: Dim ds) :: XDim xds) -> case reifyDimensions dds of
              Evidence -> case inferDimKnownDims @ds
                           +!+ inferDimFiniteList @ds of
                Evidence -> case inferArrayInstance @t @ds of
                  Evidence -> case inferNumericFrame @t @ds of
                    Evidence -> do
                      df <- peek (unsafeCoerce ptr) :: IO (DataFrame t ds)
                      return $ SChMatDF (SomeDataFrame df)


instance ( XDimensions (xd ': xds), ElemTypeInference t)
      => FromMat (MChMatDF t xd (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO $ do
      ch <- withForeignPtr (unMat m) $ \matPtr -> alloca $ \(flagsPtr :: Ptr Int32) -> do
        [CU.block|void {
          *$(int32_t *   const flagsPtr) = $(Mat * matPtr)->flags;
        }|]
        snd . unmarshalFlags <$> peek flagsPtr
      withMatData m $ \dims ptr ->
        case someDimsVal $ map fromIntegral (if ch >= 2 then fromIntegral ch : dims else dims) of
          Nothing -> error "fromMat: Could not create runtime-known Dim."
          Just (SomeDims ds) -> case xDimVal <$> wrapDim @(xd ': xds) ds of
            Nothing -> error "fromMat: Mat dimensions do not agree with the specified XDim"
            Just (XDim (dds :: Dim ds) :: XDim (xd ': xds)) -> case reifyDimensions dds of
                Evidence -> case inferDimKnownDims @ds
                             +!+ inferDimFiniteList @ds of
                  Evidence -> case inferArrayInstance @t @ds of
                    Evidence -> case inferNumericFrame @t @ds of
                      Evidence -> do
                        df <- peek (unsafeCoerce ptr) :: IO (DataFrame t ds)
                        return $ MChMatDF (SomeDataFrame df)


class DataFrameOpenCVMat shape channels t where
    matToDF :: Mat ('S shape) ('S channels) ('S t)
            -> DataFrame t (DSToXNat ('S channels ': shape) :: [DSToXNatKind shape])


instance ( FromMat (SChMatDF t (DSToXNat shape :: [DSToXNatKind shape]))
         )
      => DataFrameOpenCVMat shape 1 t where
    matToDF m
      | m' <- unsafeCoerce m
      = unSChMatDF $ fromMat m'

instance ( FromMat ( MChMatDF t
                        (Head (DSToXNat ('S c ': shape)))
                        (DSToXNat shape :: [DSToXNatKind shape])
                    )
         , (2 <=? c) ~ 'True
         )
      => DataFrameOpenCVMat shape c t where
    matToDF m
      | (Evidence :: Evidence (DSToXNat ('S c ': shape) ~ (d ': ds :: [DSToXNatKind shape])))
           <- unsafeCoerce (Evidence :: Evidence (DSToXNat ('S c ': shape) ~ DSToXNat ('S c ': shape)))
      , (Evidence :: Evidence (DSToXNat shape ~ (ds :: [DSToXNatKind shape])))
           <- unsafeCoerce (Evidence :: Evidence (DSToXNat shape ~ DSToXNat shape))
      , (Evidence :: Evidence (MatShape (MChMatDF t d ds) ~ 'S shape))
           <- unsafeCoerce (Evidence :: Evidence ('S shape ~ 'S shape))
      , (Evidence :: Evidence (MatChannels (MChMatDF t (Head (DSToXNat ('S c ': shape))) ds) ~ 'S c))
           <- unsafeCoerce (Evidence :: Evidence ('S c ~ 'S c))
      = case fromMat m of
          MChMatDF (df :: DataFrame t (d ': ds)) -> df

--
--
-- matToDF :: Mat ('S shape) channels ('S t) -> DataFrame t (channels :< shape)
-- matToDF m = undefined
