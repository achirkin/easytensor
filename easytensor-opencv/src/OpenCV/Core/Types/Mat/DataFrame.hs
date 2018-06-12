{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module OpenCV.Core.Types.Mat.DataFrame
    ( SChMatDF (..)
    , MChMatDF (..)
    , DataFrameOpenCVMat (..)
    , DimsToShape, DimsToChannels
    , ShapeChannelsToDims, ShapeChannelsToDimsKind, SnocShapeDim, SnocDSNat
    , inferDimsShapeDims1, inferDimsShapeDims
    , inferShapeDimsShape1, inferShapeDimsShape
    ) where

import           Data.Int
import           Data.Maybe
import           Data.Type.Equality
import           Data.Word
import           Foreign.Concurrent             (newForeignPtr)
import           Foreign.ForeignPtr             (withForeignPtr)
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Base                       (Type)
import           GHC.TypeLits
import qualified Language.C.Inline              as C
import qualified Language.C.Inline.Cpp          as C
import qualified Language.C.Inline.Unsafe       as CU
import           Numeric.DataFrame
import           Numeric.Dimensions             as Dim
import           Numeric.PrimBytes
import           OpenCV.Core.Types.Mat
import           OpenCV.Internal.C.Inline       (openCvCtx)
import           OpenCV.Internal.C.Types        (C'Mat)
import           OpenCV.Internal.Core.Types.Mat (Mat (..), unmarshalFlags,
                                                 withMatData)
import           OpenCV.TypeLevel               as OpenCV
import           System.IO.Unsafe
import           Unsafe.Coerce                  (unsafeCoerce)

C.context openCvCtx

#include "MachDeps.h"
C.include "opencv2/core.hpp"
C.using "namespace cv"

-- | DataFrame as a Single Channel Matrix
newtype SChMatDF (t :: Type) (ds :: [k])
  = SChMatDF { unSChMatDF :: DataFrame t ds }

-- | DataFrame as a Multiple Channel Matrix
newtype MChMatDF (t :: Type) (d :: k) (ds :: [k])
  = MChMatDF { unMChMatDF :: DataFrame t (d ': ds) }


class XDimensions (xds :: [XNat]) where
    wrapDims :: Dims (ds :: [Nat]) -> Maybe (Dims xds)


instance XDimensions '[] where
    wrapDims U = Just U
    {-# INLINE wrapDims #-}

instance (XDimensions xs, KnownDim m) => XDimensions (XN m ': xs) where
    wrapDims (d :* ds) = case constrain d of
      Nothing -> Nothing
      Just xd -> (xd :*) <$> wrapDims ds
    wrapDims Empty = Nothing

instance (XDimensions xs, KnownDim n) => XDimensions (N n ': xs) where
    wrapDims (d :* ds) = case sameDim d (Dim.D @n) of
      Nothing -> Nothing
      Just E  -> (Dn Dim.D :*) <$> wrapDims ds
    wrapDims Empty = Nothing


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
    SnocShapeDim ('[]          :: [XNat]) ('S x) = ( '[N x] :: [XNat] )
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
inferDimsShapeDims1 = unsafeCoerce (E :: Evidence (ds ~ ds, k ~ k))

inferDimsShapeDims :: forall (d :: k) (ds :: [k])
                    . Evidence
                    ( ShapeChannelsToDims     (DimsToShape 'True (d ': ds)) (DimsToChannels 'True (d ': ds)) ~~ (d ': ds)
                    , ShapeChannelsToDimsKind (DimsToShape 'True (d ': ds)) (DimsToChannels 'True (d ': ds)) ~ k
                    )
inferDimsShapeDims = unsafeCoerce (E :: Evidence (ds ~ ds, k ~ k))

inferShapeDimsShape1 :: forall shape
                      . Evidence
                      ( DimsToShape    'False (ShapeChannelsToDims shape ('S 1)) ~ shape
                      , DimsToChannels 'False (ShapeChannelsToDims shape ('S 1)) ~ 'S 1
                      )
inferShapeDimsShape1 = unsafeCoerce (E :: Evidence (shape ~ shape, 'S 1 ~ 'S 1))

inferShapeDimsShape :: forall shape channels
                     . Evidence
                     ( DimsToShape    'True (ShapeChannelsToDims shape channels) ~ shape
                     , DimsToChannels 'True (ShapeChannelsToDims shape channels) ~ channels
                     )
inferShapeDimsShape = unsafeCoerce (E :: Evidence (shape ~ shape, channels ~ channels))


type instance MatShape (SChMatDF t ds) = 'S (DimsToShape 'False ds)
type instance MatShape (MChMatDF t d ds) = 'S (DimsToShape 'True (d ': ds))

type instance MatChannels (SChMatDF t ds) = DimsToChannels 'False ds
type instance MatChannels (MChMatDF t c ds) = DimsToChannels 'True (c ': ds)

type instance MatDepth (SChMatDF t ds)   = 'S t
type instance MatDepth (MChMatDF t d ds) = 'S t


openCVType :: PrimTag t -> C.CInt -> IO C.CInt
openCVType PTagFloat  channels = [CU.exp| int { CV_32FC($(int channels)) } |]
openCVType PTagDouble channels = [CU.exp| int { CV_64FC($(int channels)) } |]
#if WORD_SIZE_IN_BITS < 64
openCVType PTagInt    channels = [CU.exp| int { CV_32SC($(int channels)) } |]
#else
openCVType PTagInt    _        = return 0
#endif
openCVType PTagInt8   channels = [CU.exp| int { CV_8SC($(int channels)) } |]
openCVType PTagInt16  channels = [CU.exp| int { CV_16SC($(int channels)) } |]
openCVType PTagInt32  channels = [CU.exp| int { CV_32SC($(int channels)) } |]
openCVType PTagInt64  _        = return 0
openCVType PTagWord   _        = return 0
openCVType PTagWord8  channels = [CU.exp| int { CV_8UC($(int channels)) } |]
openCVType PTagWord16 channels = [CU.exp| int { CV_16UC($(int channels)) } |]
openCVType PTagWord32 _        = return 0
openCVType PTagWord64 _        = return 0
openCVType PTagPtr    _        = return 0
openCVType PTagOther  _        = return 0

finMatPtr :: Ptr C'Mat -> IO ()
finMatPtr rPtr = [CU.exp| void { delete $(Mat * rPtr) }|]


instance (PrimBytes t, Storable (DataFrame t ds), Dimensions ds) =>
       ToMat (SChMatDF t (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (SChMatDF df) = unsafePerformIO $ makeMat @'False @t @1 (dims @_ @ds) df

instance PrimBytes t => ToMat (SChMatDF t (xds :: [XNat])) where
  toMat (SChMatDF (XFrame (df :: DataFrame t ds)))
    | (E :: Evidence (DimsToShape 'False ds ~ DimsToShape 'False xds))
          <- unsafeCoerce (E :: Evidence (DimsToShape 'False xds ~ DimsToShape 'False xds))
    , E <- inferASing df
    , E <- inferPrim df
    = toMat $ SChMatDF df

instance (Dimensions ds, KnownDim d, Storable (DataFrame t (d ': ds)), PrimBytes t)
         => ToMat (MChMatDF t d (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (MChMatDF df) = case dims @_ @(d ': ds) of
    _ :* ds -> unsafePerformIO $ makeMat  @'True @t @d ds df


instance ToMat (MChMatDF t xd (xds :: [XNat])) where
  toMat (MChMatDF (XFrame (df :: DataFrame t dds)))
    | (E :: Evidence (dds ~ (n ': ns))) <- unsafeCoerce (E :: Evidence (dds ~ dds))
    , (E :: Evidence (DimsToShape 'True (n ': ns) ~ DimsToShape 'True (xd ': xds)))
          <- unsafeCoerce (E :: Evidence (DimsToShape 'True (xd ': xds) ~ DimsToShape 'True (xd ': xds)))
    , (E :: Evidence (DimsToChannels 'True (n ': ns) ~ DimsToChannels 'True (xd ': xds)))
          <- unsafeCoerce (E :: Evidence (DimsToChannels 'True (xd ': xds) ~ DimsToChannels 'True (xd ': xds)))
    , Dim.D :* Dims <- dims @_ @dds
    , E <- inferPrimElem df
    , E <- inferPrim df
    = toMat $ MChMatDF df
  toMat _ = error "toMat: impossible arguments"

instance Storable (DataFrame t ds)
      => FromMat (SChMatDF t (ds :: [Nat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO . withMatData m $ \_ -> fmap SChMatDF . peek . unsafeCoerce

instance Storable (DataFrame t (d ': ds))
      => FromMat (MChMatDF t d (ds :: [Nat])) where
    {-# NOINLINE fromMat #-}
    fromMat m = unsafePerformIO . withMatData m $ \_ -> fmap MChMatDF . peek . unsafeCoerce

instance ( XDimensions xds, KnownXNatTypes xds, PrimBytes t)
         => FromMat (SChMatDF t (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | E <- inferDimsShapeDims1 @XNat @xds
      = unsafePerformIO . withMatData' m $ \xds -> fmap SChMatDF . readMatData @t xds

instance ( XDimensions (xd ': xds), KnownXNatTypes (xd ': xds), PrimBytes t)
         => FromMat (MChMatDF t xd (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | E <- inferDimsShapeDims @XNat @xd @xds
      = unsafePerformIO . withMatData' m $ \xds -> fmap MChMatDF . readMatData @t xds


class ( ShapeChannelsToDimsKind shape channels ~ k
      , ShapeChannelsToDims shape channels ~~ ds
      )
      => DataFrameOpenCVMat shape channels t k (ds :: [k]) | -- TODO ds channels -> shape
                                                             -- ,
                                                            --  ds shape -> channels
                                                           -- ,
                                                           shape channels k -> ds
                                                           where
    matToDF :: Mat ('S shape) channels ('S t)
            -> DataFrame t (ds :: [k])

instance {-# OVERLAPS #-}
         ( Storable (DataFrame t ds)
         , ShapeChannelsToDimsKind shape channels ~ Nat
         , ShapeChannelsToDims shape channels ~~ ds
         )
      => DataFrameOpenCVMat shape channels t Nat ds where
    matToDF m = unsafePerformIO . withMatData m $ \_ -> peek . unsafeCoerce


instance {-# OVERLAPS #-}
         ( XDimensions xds
         , ShapeChannelsToDimsKind shape channels ~ XNat
         , ShapeChannelsToDims shape channels ~~ xds
         , KnownXNatTypes xds
         , PrimBytes t
         )
      => DataFrameOpenCVMat shape channels t XNat xds where
    matToDF m
      = unsafePerformIO . withMatData' m $ readMatData @t




makeMat :: forall multi (t :: Type) (d :: Nat) (ds :: [Nat]) (ds' :: [Nat])
         . ( Storable (DataFrame t ds)
           , PrimBytes t
           , KnownDim d
           )
        => Dims ds' -> DataFrame t ds -> IO (Mat ('S (DimsToShape multi ds)) (DimsToChannels multi ds) ('S t))
makeMat ds df = do
    dataPtr <- malloc
    shapePtr <- mallocArray dimN
    poke dataPtr df
    writeDims shapePtr (dimN - 1) ds
    cvType <- openCVType (primTag @t undefined) (fromIntegral $ dimVal' @d )
    let ptr' = unsafeCoerce dataPtr :: Ptr ()
    rPtr <- [CU.exp| Mat * { new cv::Mat( $(int cdimN), $(int * shapePtr), $(int cvType), $(void * ptr')) } |]
    Mat <$> newForeignPtr rPtr (finMatPtr rPtr >> free dataPtr >> free shapePtr)
  where
    dimN = fromIntegral . dimVal $ order ds
    cdimN = fromIntegral dimN :: C.CInt

readMatData :: forall t (xds :: [XNat])
             . ( PrimBytes t
               , KnownXNatTypes xds
               )
            => Dims xds -> Ptr Word8 -> IO (DataFrame t xds)
readMatData (XDims (_ :: Dims ds)) ptr
  | E <- inferASing' @t @ds
  , E <- inferPrim' @t @ds
  = do df <- peek (unsafeCoerce ptr) :: IO (DataFrame t ds)
       return $ XFrame df


withMatData' :: ( XDimensions xds
                , ShapeChannelsToDims shape channels ~~ xds
                )
             => Mat ('S shape) channels depth -> (Dims xds -> Ptr Word8 -> IO b) -> IO b
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
      ds       <- readDims channels shapePtr (fromIntegral dimN)
      f ds dataPtr



writeDims :: forall (xs :: [k]) . Ptr C.CInt -> Int -> Dims xs -> IO ()
writeDims _ _ U = return ()
writeDims p i (d :* ds) = pokeElemOff p i (fromIntegral $ dimVal d) >> writeDims p (i-1) ds

readDims  :: forall (xds :: [XNat])
          . XDimensions xds
          => Int32 -> Ptr Int32 -> Int -> IO (Dims xds)
readDims channels ptr n = do
    xs <- map fromIntegral . withChannels channels . reverse <$> peekArray n ptr
    return $ case someDimsVal xs of
      SomeDims xds -> fromMaybe
         (error "readDims: read dimensions do not agree with the specified XDim")
         (wrapDims @xds xds)
  where
    withChannels c xs = if c > 1 then c : xs else xs
