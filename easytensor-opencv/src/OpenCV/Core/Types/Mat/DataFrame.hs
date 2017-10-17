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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module OpenCV.Core.Types.Mat.DataFrame
    ( SChMatDF (..)
    , MChMatDF (..)
    , DataFrameOpenCVMat (..)
    , DimsToShape, DimsToChannels
    , ShapeChannelsToDims, ShapeChannelsToDimsKind, SnocShapeDim, SnocDSNat
    , inferDimsShapeDims1, inferDimsShapeDims
    , inferShapeDimsShape1, inferShapeDimsShape
    ) where

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import OpenCV.Internal.C.Inline ( openCvCtx )
import OpenCV.Internal.C.Types (C'Mat)
import OpenCV.Internal.Core.Types.Mat (withMatData, Mat (..), unmarshalFlags)
import OpenCV.Core.Types.Mat
import OpenCV.TypeLevel as OpenCV
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)
import Data.Int
import Data.Word
import Data.Type.Equality

import Numeric.DataFrame
import Numeric.Dimensions as Dim
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

-- | Type-level function to correspond easytensor's dimensions
--   and opencv shape and channel information.
--   This mapping could be ambiguous under certain circumstances;
--   thus, the closed type family has been introduced to make it straightforward.
--
--   * OpenCV and eastensor have different data layout;
--       Mat is row-first, DataFrame is column-first.
--   * Nevertheless, multiple channels in the Mat are treated as the first
--       dimension in the DataFrame.
type family DimsToShape (multi :: Bool) (ds :: [k]) :: [DS Nat] where
    DimsToShape 'False ('[]          :: [Nat] ) = '[]
    DimsToShape 'False ((n ': ns)    :: [Nat] ) = SnocDSNat (DimsToShape 'False ns) ('S n)
    DimsToShape 'False ('[]          :: [XNat]) = '[]
    DimsToShape 'False ((N  n ': ns) :: [XNat]) = SnocDSNat (DimsToShape 'False ns) ('S n)
    DimsToShape 'False ((XN _ ': ns) :: [XNat]) = SnocDSNat (DimsToShape 'False ns) 'OpenCV.D
    DimsToShape 'True  ('[]          :: [Nat] ) = '[]
    DimsToShape 'True  ('[]          :: [XNat]) = '[]
    DimsToShape 'True  ((_ ': ns)    :: [Nat] ) = DimsToShape 'False ns
    DimsToShape 'True  ((_ ': ns)    :: [XNat]) = DimsToShape 'False ns
    DimsToShape  multi dims                     = TypeError
      ( 'Text "DimsToShape: unknown combination of parameters:"
      ':$$: 'Text "multi-channel = " ':<>: 'ShowType multi
      ':$$: 'Text "dimensions    = " ':<>: 'ShowType dims
      )

type family DimsToChannels (multi :: Bool) (ds :: [k]) :: DS Nat where
    DimsToChannels 'False (_            :: [Nat] ) = 'S 1
    DimsToChannels 'False (_            :: [XNat]) = 'S 1
    DimsToChannels 'True  ('[]          :: [Nat] ) = 'S 1
    DimsToChannels 'True  ('[]          :: [XNat]) = 'S 1
    DimsToChannels 'True  ((c ': ns)    :: [Nat] ) = 'S c
    DimsToChannels 'True  ((N c  ': ns) :: [XNat]) = 'S c
    DimsToChannels 'True  ((XN _ ': ns) :: [XNat]) = 'OpenCV.D
    DimsToChannels  multi  dims                    = TypeError
      ( 'Text "DimsToChannels: unknown combination of parameters:"
      ':$$: 'Text "multi-channel = " ':<>: 'ShowType multi
      ':$$: 'Text "dimensions    = " ':<>: 'ShowType dims
      )

type family ShapeChannelsToDims (shape :: [DS Nat]) (channels :: DS Nat) :: [ShapeChannelsToDimsKind shape channels] where
    ShapeChannelsToDims '[] ('S 1)      = ('[] :: [Nat])
    ShapeChannelsToDims '[] ('S c)      = ('[c] :: [Nat])
    ShapeChannelsToDims '[] ('OpenCV.D) = ('[XN 1] :: [XNat])
    ShapeChannelsToDims (d ': ds) c     = (SnocShapeDim (ShapeChannelsToDims ds c :: [ShapeChannelsToDimsKind ds c]) d)


type family SnocShapeDim (ds :: [k]) (x :: DS Nat) :: [l] where
    SnocShapeDim ('[]          :: [Nat] ) ('S x) = ( '[x] :: [Nat] )
    SnocShapeDim ('[]          :: [XNat]) ('S x) = ( '[x] :: [Nat] )
    SnocShapeDim ((n    ': ns) :: [Nat] ) ('S x) = ( n    ': SnocShapeDim ns ('S x) :: [Nat]  )
    SnocShapeDim ((N  n ': ns) :: [XNat]) ('S x) = ( N  n ': SnocShapeDim ns ('S x) :: [XNat] )
    SnocShapeDim ((XN m ': ns) :: [XNat]) ('S x) = ( XN m ': SnocShapeDim ns ('S x) :: [XNat] )
    SnocShapeDim ('[]          :: [Nat] ) 'OpenCV.D = ( '[XN 1] :: [XNat] )
    SnocShapeDim ('[]          :: [XNat]) 'OpenCV.D = ( '[XN 1] :: [XNat] )
    SnocShapeDim ((n    ': ns) :: [Nat] ) 'OpenCV.D = ( N  n ': SnocShapeDim ns 'OpenCV.D :: [XNat] )
    SnocShapeDim ((N  n ': ns) :: [XNat]) 'OpenCV.D = ( N  n ': SnocShapeDim ns 'OpenCV.D :: [XNat] )
    SnocShapeDim ((XN m ': ns) :: [XNat]) 'OpenCV.D = ( XN m ': SnocShapeDim ns 'OpenCV.D :: [XNat] )
    SnocShapeDim   dims                   shape     = TypeError
      ( 'Text "ShapeChannelsToDims-snoc: unknown combination of parameters:"
      ':$$: 'Text "dimensions = " ':<>: 'ShowType dims
      ':$$: 'Text "snoc-ed DS = " ':<>: 'ShowType shape
      )

type family SnocDSNat (ds :: [DS Nat]) (z :: DS Nat) :: [DS Nat] where
    SnocDSNat '[] z = '[z]
    SnocDSNat (d ': ds) z = d ': SnocDSNat ds z

type family ShapeChannelsToDimsKind (shape :: [DS Nat]) (channels :: DS Nat) :: k where
    ShapeChannelsToDimsKind   _     'OpenCV.D  = XNat
    ShapeChannelsToDimsKind ('OpenCV.D ': _) _ = XNat
    ShapeChannelsToDimsKind  '[]    ('S _)     = Nat
    ShapeChannelsToDimsKind ('S _ ': xs)    c  = ShapeChannelsToDimsKind xs c



inferDimsShapeDims1 :: forall (ds :: [k])
                     . Evidence
                     ( ShapeChannelsToDims     (DimsToShape 'False ds) (DimsToChannels 'False ds) ~~ ds
                     , ShapeChannelsToDimsKind (DimsToShape 'False ds) (DimsToChannels 'False ds) ~ k
                     )
inferDimsShapeDims1 = unsafeCoerce (Evidence :: Evidence (ds ~ ds, k ~ k))

inferDimsShapeDims :: forall (d :: k) (ds :: [k])
                    . Evidence
                    ( ShapeChannelsToDims     (DimsToShape 'True (d ': ds)) (DimsToChannels 'True (d ': ds)) ~~ (d ': ds)
                    , ShapeChannelsToDimsKind (DimsToShape 'True (d ': ds)) (DimsToChannels 'True (d ': ds)) ~ k
                    )
inferDimsShapeDims = unsafeCoerce (Evidence :: Evidence (ds ~ ds, k ~ k))

inferShapeDimsShape1 :: forall shape
                      . Evidence
                      ( DimsToShape    'False (ShapeChannelsToDims shape ('S 1)) ~ shape
                      , DimsToChannels 'False (ShapeChannelsToDims shape ('S 1)) ~ 'S 1
                      )
inferShapeDimsShape1 = unsafeCoerce (Evidence :: Evidence (shape ~ shape, 'S 1 ~ 'S 1))

inferShapeDimsShape :: forall shape channels
                     . Evidence
                     ( DimsToShape    'True (ShapeChannelsToDims shape channels) ~ shape
                     , DimsToChannels 'True (ShapeChannelsToDims shape channels) ~ channels
                     )
inferShapeDimsShape = unsafeCoerce (Evidence :: Evidence (shape ~ shape, channels ~ channels))


type instance MatShape (SChMatDF t ds) = 'S (DimsToShape 'False ds)
type instance MatShape (MChMatDF t d ds) = 'S (DimsToShape 'True (d ': ds))

type instance MatChannels (SChMatDF t ds) = DimsToChannels 'False ds
type instance MatChannels (MChMatDF t c ds) = DimsToChannels 'True (c ': ds)

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

finMatPtr :: Ptr C'Mat -> IO ()
finMatPtr rPtr = [CU.exp| void { delete $(Mat * rPtr) }|]


instance NumericFrame t ds
       => ToMat (SChMatDF t (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (SChMatDF df) = unsafePerformIO $ makeMat @'False @t @1 (dim @ds) df

instance ToMat (SChMatDF t (xds :: [XNat])) where
  toMat (SChMatDF (SomeDataFrame (df :: DataFrame t ds)))
    | (Evidence :: Evidence (DimsToShape 'False ds ~ DimsToShape 'False xds))
          <- unsafeCoerce (Evidence :: Evidence (DimsToShape 'False xds ~ DimsToShape 'False xds))
    = toMat $ SChMatDF df

instance NumericFrame t (d ': ds)
       => ToMat (MChMatDF t d (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (MChMatDF df) = case dim @(d ': ds) of
    _ :* ds -> unsafePerformIO $ makeMat  @'True @t @d ds df


instance ToMat (MChMatDF t xd (xds :: [XNat])) where
  toMat (MChMatDF (SomeDataFrame (df :: DataFrame t dds)))
    | (Evidence :: Evidence (dds ~ (n ': ns))) <- unsafeCoerce (Evidence :: Evidence (dds ~ dds))
    , (Evidence :: Evidence (DimsToShape 'True (n ': ns) ~ DimsToShape 'True (xd ': xds)))
          <- unsafeCoerce (Evidence :: Evidence (DimsToShape 'True (xd ': xds) ~ DimsToShape 'True (xd ': xds)))
    , (Evidence :: Evidence (DimsToChannels 'True (n ': ns) ~ DimsToChannels 'True (xd ': xds)))
          <- unsafeCoerce (Evidence :: Evidence (DimsToChannels 'True (xd ': xds) ~ DimsToChannels 'True (xd ': xds)))
    = toMat $ MChMatDF df

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
    fromMat m
      | Evidence <- inferDimsShapeDims1 @XNat @xds
      = unsafePerformIO . withMatData' m $ \dims -> fmap SChMatDF . readMatData @t (xDimVal dims)

instance ( XDimensions (xd ': xds), ElemTypeInference t)
      => FromMat (MChMatDF t xd (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | Evidence <- inferDimsShapeDims @XNat @xd @xds
      = unsafePerformIO . withMatData' m $ \dims -> fmap MChMatDF . readMatData @t (xDimVal dims)


class DataFrameOpenCVMat shape channels t k (ds :: [k]) | ds channels -> shape
                                                        , ds shape -> channels
                                                        where
    matToDF :: Mat ('S shape) channels ('S t)
            -> DataFrame t (ds :: [k])

instance ( Storable (DataFrame t ds)
         , ShapeChannelsToDimsKind shape channels ~ Nat
         , ShapeChannelsToDims shape channels ~~ ds
         )
      => DataFrameOpenCVMat shape channels t Nat ds where
    matToDF m = unsafePerformIO . withMatData m $ \_ -> peek . unsafeCoerce


instance ( XDimensions xds
         , ShapeChannelsToDimsKind shape channels ~ XNat
         , ShapeChannelsToDims shape channels ~~ xds
         , ElemTypeInference t
         )
      => DataFrameOpenCVMat shape channels t XNat xds where
    matToDF m
      = unsafePerformIO . withMatData' m $ \dims -> readMatData @t (xDimVal dims)




makeMat :: forall multi t (d :: Nat) (ds :: [Nat]) (ds' :: [Nat])
         . ( Storable (DataFrame t ds)
           , ElemTypeInference t
           , KnownDim d
           )
        => Dim ds' -> DataFrame t ds -> IO (Mat ('S (DimsToShape multi ds)) (DimsToChannels multi ds) ('S t))
makeMat dims df = do
    dataPtr <- malloc
    shapePtr <- mallocArray dimN
    poke dataPtr df
    writeDim shapePtr (dimN - 1) dims
    cvType <- openCVType (elemTypeInstance @t) (fromIntegral $ dimVal' @d )
    let ptr' = unsafeCoerce dataPtr :: Ptr ()
    rPtr <- [CU.exp| Mat * { new cv::Mat( $(int cdimN), $(int * shapePtr), $(int cvType), $(void * ptr')) } |]
    Mat <$> newForeignPtr rPtr (finMatPtr rPtr >> free dataPtr >> free shapePtr)
  where
    dimN = order' dims
    cdimN = fromIntegral dimN :: C.CInt

readMatData :: forall t xds
             . ( ElemTypeInference t
               , XDimensions xds)
            => XDim xds -> Ptr Word8 -> IO (DataFrame t xds)
readMatData (XDim (dims :: Dim ds)) ptr = case reifyDimensions dims of
  Evidence -> case inferDimKnownDims @ds
               +!+ inferDimFiniteList @ds of
    Evidence -> case inferArrayInstance @t @ds of
      Evidence -> case inferNumericFrame @t @ds of
        Evidence -> do
          df <- peek (unsafeCoerce ptr) :: IO (DataFrame t ds)
          return $ SomeDataFrame df


withMatData' :: ( XDimensions xds
                , ShapeChannelsToDims shape channels ~~ xds
                )
             => Mat ('S shape) channels depth -> (Dim xds -> Ptr Word8 -> IO b) -> IO b
withMatData' m f = withForeignPtr (unMat m) $ \matPtr ->
    alloca $ \(orderPtr  :: Ptr Int32      ) ->
    alloca $ \(flagsPtr  :: Ptr Int32      ) ->
    alloca $ \(shapePtr2 :: Ptr (Ptr Int32)) ->
    alloca $ \(dataPtr2  :: Ptr (Ptr Word8)) -> do
      [CU.block|void {
        const Mat * const matPtr = $(Mat * matPtr);
        *$(int32_t *   const flagsPtr)  = matPtr->flags;
        *$(int32_t *   const orderPtr ) = matPtr->dims;
        *$(int32_t * * const shapePtr2) = matPtr->size.p;
        *$(uint8_t * * const dataPtr2 ) = matPtr->data;
      }|]
      (_, channels) <- unmarshalFlags <$> peek flagsPtr
      dimN     <- peek orderPtr
      shapePtr <- peek shapePtr2
      dataPtr  <- peek dataPtr2
      dims     <- readDim channels shapePtr (fromIntegral dimN)
      f dims dataPtr


order' :: Dim ds -> Int
order' Dim.D  = 0
order'  Dn    = 1
order' (Dx _) = 1
order' (d :* ds) = order' d + order' ds

writeDim :: forall (xs :: [k]) . Ptr C.CInt -> Int -> Dim xs -> IO ()
writeDim _ _ Dim.D = return ()
writeDim p i (d :* ds) = pokeElemOff p i (fromIntegral $ dimVal d) >> writeDim p (i-1) ds

readDim  :: forall (xds :: [XNat])
          . XDimensions xds
         => Int32 -> Ptr Int32 -> Int -> IO (Dim xds)
readDim channels ptr n = do
    xs <- map fromIntegral . withChannels channels . reverse <$> peekArray n ptr
    return $ case someDimsVal xs of
      Nothing -> error "readDim: Could not create runtime-known Dim."
      Just (SomeDims ds) -> case wrapDim @xds ds of
        Nothing -> error "readDim: read dimensions do not agree with the specified XDim"
        Just dims -> dims
  where
    withChannels c xs = if c > 1 then c : xs else xs
