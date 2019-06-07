
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
module OpenCV.Core.Types.Mat.DataFrame
  ( SChMatDF (..), MChMatDF (..), XNatsToDSs
  , FixDepth (..), fixDepth
  , FixChannels (..), fixChannels
  , FixShape (..), fixShape
  , PrimTag (..), primTag
  ) where

import           Data.Int
import           Data.Maybe
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
import           Numeric.DataFrame              hiding (D, S)
import           Numeric.Dimensions             hiding (D)
import qualified Numeric.Dimensions             as Dim
import           Numeric.PrimBytes
import           OpenCV.Core.Types.Mat          hiding (All)
import           OpenCV.Internal.C.Inline       (openCvCtx)
import           OpenCV.Internal.C.Types        (C'Mat)
import           OpenCV.Internal.Core.Types.Mat (Mat (..), unmarshalFlags,
                                                 withMatData)
import           OpenCV.TypeLevel               as OpenCV hiding (All)
import           OpenCV.Unsafe                  (unsafeCoerceMat)
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
newtype MChMatDF (t :: Type) (ds :: [k]) (d :: k)
  = MChMatDF { unMChMatDF :: DataFrame t (ds +: d) }

type instance MatDepth (SChMatDF (t :: Type) (ds :: [k])) = 'S t
type instance MatDepth (MChMatDF (t :: Type) (ds :: [k]) (d :: k)) = 'S t

type instance MatShape (SChMatDF (t :: Type) (ds :: [Nat])) = 'S (Map 'S ds)
type instance MatShape (SChMatDF (t :: Type) (ds :: [XNat])) = 'S (XNatsToDSs ds)
type instance MatShape (MChMatDF (t :: Type) (ds :: [Nat]) (d :: Nat)) = 'S (Map 'S ds)
type instance MatShape (MChMatDF (t :: Type) (ds :: [XNat]) (d :: XNat)) = 'S (XNatsToDSs ds)

type instance MatChannels (SChMatDF (t :: Type) (ds :: [k])) = 'S 1
type instance MatChannels (MChMatDF (t :: Type) (ds :: [Nat]) n) = 'S n
type instance MatChannels (MChMatDF (t :: Type) (ds :: [XNat]) ('N n)) = 'S n
type instance MatChannels (MChMatDF (t :: Type) (ds :: [XNat]) ('XN m)) = 'D

-- | Mapping from @easytensor@'s @XNat@s and @opencv@'s @DS@.
--
--   Basically, this is a one-to-one mapping, except the minimum @Nat@ value
--   constraint @m@ from @XN m@ is lost.
type family XNatsToDSs  (xns :: [XNat]) :: [DS Nat] where
  XNatsToDSs '[]            = '[]
  XNatsToDSs ('XN _ ': xns) = 'D   ': XNatsToDSs xns
  XNatsToDSs ('N n  ': xns) = 'S n ': XNatsToDSs xns


instance (PrimBytes t, Dimensions ds)
         => ToMat (SChMatDF t (ds :: [Nat])) where
  {-# NOINLINE toMat #-}
  toMat (SChMatDF df) = unsafePerformIO $ makeMat @t @ds @1 dims df

instance PrimBytes t
         => ToMat (SChMatDF t (xds :: [XNat])) where
  toMat (SChMatDF (XFrame (df :: DataFrame t ds)))
      -- this follows from FixedDim
    | Dict <- unsafeEqTypes @_ @'True @(Relax ('S (Map 'S ds)) ('S (XNatsToDSs xds)))
      = relaxMat . toMat $ SChMatDF df

instance (Dimensions ds, KnownDim d, PrimBytes t)
         => ToMat (MChMatDF t (ds :: [Nat]) (d :: Nat)) where
  {-# NOINLINE toMat #-}
  toMat (MChMatDF df) = unsafePerformIO $ makeMat @t @ds @d dims df

instance PrimBytes t
         => ToMat (MChMatDF t (xds :: [XNat]) (xd :: XNat)) where
  toMat (MChMatDF (XFrame (df :: DataFrame t dsd)))
      | Snoc (Dims :: Dims ds) (Dim.D :: Dim d) <- dims @dsd
      , Dict <- unsafeEqTypes @_ @'True @(Relax ('S d) (MatChannels (MChMatDF t xds xd)))
      , Dict <- unsafeEqTypes @_ @'True @(Relax ('S (Map 'S ds)) ('S (XNatsToDSs xds)))
      = relaxMat . toMat $ MChMatDF df
  toMat _ = error "OpenCV.Core.Types.Mat.DataFrame.toMat: impossible arguments"


instance (PrimBytes t, Dimensions ds)
         => FromMat (SChMatDF t (ds :: [Nat])) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | Dict <- inferKnownBackend @t @ds
      = unsafePerformIO . withMatData m $ \_ -> fmap SChMatDF . peek . unsafeCoerce


instance (PrimBytes t, Dimensions ds, KnownDim d)
          => FromMat (MChMatDF t (ds :: [Nat]) (d :: Nat)) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | Dims <- Snoc (dims @ds) (Dim.D @d)
      , Dict <- inferKnownBackend @t @(ds +: d)
      = unsafePerformIO . withMatData m $ \_ -> fmap MChMatDF . peek . unsafeCoerce
    fromMat _ = error "OpenCV.Core.Types.Mat.DataFrame.fromMat: impossible arguments"


instance ( PrimBytes t, BoundedDims xds, KnownXNatTypes xds)
         => FromMat (SChMatDF t (xds :: [XNat])) where
    {-# NOINLINE fromMat #-}
    fromMat m
      = unsafePerformIO . withMatData' m $ \xds -> fmap SChMatDF . readMatData @t xds

instance ( PrimBytes t, BoundedDims xds, BoundedDim xd
         , KnownXNatTypes xds, KnownXNatType xd)
         => FromMat (MChMatDF t (xds :: [XNat]) (xd :: XNat)) where
    {-# NOINLINE fromMat #-}
    fromMat m
      | Dict <- inferAllBoundedDims @XNat @xds
      , Dict <- snocBDims @xds @xd minDims minDim
      = unsafePerformIO . withMatData' m $ \xds -> fmap MChMatDF . readMatData @t xds
      where
        snocBDims :: forall (xns :: [XNat]) (xn :: XNat)
                   . ( All BoundedDim xns, BoundedDim xn
                     , KnownXNatTypes xns, KnownXNatType xn)
                  => Dims xns -> Dim xn -> Dict (BoundedDims (xns +: xn), KnownXNatTypes (xns +: xn))
        snocBDims U _           = Dict
        snocBDims (_ :* xns) xn = case snocBDims xns xn of Dict -> Dict


-- | Reveal the Mat depth by pattern-matching against this data
data FixDepth (shape :: DS [DS Nat]) (channels :: DS Nat)
  = forall (t :: Type) . PrimBytes t => FixDepth (PrimTag t) (Mat shape channels ('S t))

-- | Reveal the Mat depth by pattern-matching against this data
fixDepth ::
     forall (shape :: DS [DS Nat]) (channels :: DS Nat) (depth :: DS Type)
   . Mat shape channels depth -> FixDepth shape channels
fixDepth m = case miDepth (matInfo m) of
    Depth_8U       -> FixDepth PTagWord8  (unsafeCoerceMat m)
    Depth_8S       -> FixDepth PTagInt8   (unsafeCoerceMat m)
    Depth_16U      -> FixDepth PTagWord16 (unsafeCoerceMat m)
    Depth_16S      -> FixDepth PTagInt16  (unsafeCoerceMat m)
    Depth_32S      -> FixDepth PTagInt32  (unsafeCoerceMat m)
    Depth_32F      -> FixDepth PTagFloat  (unsafeCoerceMat m)
    Depth_64F      -> FixDepth PTagDouble (unsafeCoerceMat m)
    Depth_USRTYPE1 -> error "OpenCV.Core.Types.Mat.DataFrame.fixDepth: Depth_USRTYPE1"

-- | Reveal the Mat channels by pattern-matching against this data
data FixChannels (shape :: DS [DS Nat]) (depth :: DS Type)
  = forall (c :: Nat) . KnownDim c => FixChannels (Dim c) (Mat shape ('S c) depth)

-- | Reveal the Mat channels by pattern-matching against this data
fixChannels ::
     forall (shape :: DS [DS Nat]) (channels :: DS Nat) (depth :: DS Type)
   . Mat shape channels depth -> FixChannels shape depth
fixChannels m = case someDimVal (fromIntegral $ miChannels (matInfo m)) of
    Dx (d@Dim.D :: Dim n)
      -> FixChannels d (unsafeCoerceMat m :: Mat shape ('S n) depth)

-- | Reveal the Mat shape by pattern-matching against this data
data FixShape (channels :: DS Nat) (depth :: DS Type)
  = forall (ds :: [Nat]) . Dimensions ds
    => FixShape (Dims ds) (Mat ('S (Map 'S ds)) channels depth)

-- | Reveal the Mat shape by pattern-matching against this data
fixShape ::
     forall (shape :: DS [DS Nat]) (channels :: DS Nat) (depth :: DS Type)
   . Mat shape channels depth -> FixShape channels depth
fixShape m = case someDimsVal (fromIntegral <$> miShape (matInfo m)) of
    SomeDims (ds@Dims :: Dims ds)
      -> FixShape ds (unsafeCoerceMat m :: Mat ('S (Map 'S ds)) channels depth)




--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------


-- | Compute the OpenCV element type tag given the type of an element and the
--   number of channels
openCVType :: PrimTag t -> C.CInt -> IO C.CInt
openCVType PTagFloat  channels = [CU.exp| int { CV_32FC($(int channels)) } |]
openCVType PTagDouble channels = [CU.exp| int { CV_64FC($(int channels)) } |]
#if WORD_SIZE_IN_BITS < 64
openCVType PTagInt    channels = [CU.exp| int { CV_32SC($(int channels)) } |]
#else
openCVType PTagInt    _        = return 0
#endif
openCVType PTagInt8   channels = [CU.exp| int { CV_8SC($(int channels))  } |]
openCVType PTagInt16  channels = [CU.exp| int { CV_16SC($(int channels)) } |]
openCVType PTagInt32  channels = [CU.exp| int { CV_32SC($(int channels)) } |]
openCVType PTagInt64  _        = return 0
openCVType PTagWord   _        = return 0
openCVType PTagWord8  channels = [CU.exp| int { CV_8UC($(int channels))  } |]
openCVType PTagWord16 channels = [CU.exp| int { CV_16UC($(int channels)) } |]
openCVType PTagWord32 _        = return 0
openCVType PTagWord64 _        = return 0
openCVType PTagChar   _        = return 0
openCVType PTagPtr    _        = return 0
openCVType PTagOther  _        = return 0


-- | This function is unsafe in that it does not enforce the invariant:
--
--   if channels > 1 then ds' ~ (ds +: channels) else ds' ~ ds
makeMat :: forall (t :: Type) (ds :: [Nat]) (channels :: Nat) (ds' :: [Nat])
         . (PrimBytes t, KnownDim channels)
        => Dims ds -> DataFrame t ds' -> IO (Mat ('S (Map 'S ds)) ('S channels) ('S t))
makeMat ds@Dims df
  | Dims <- ds'
  , Dict <- inferKnownBackend @t @ds'
    = do
    dataPtr <- malloc
    shapePtr <- mallocArray dimN
    poke dataPtr df
    writeDims shapePtr ds
    cvType <- openCVType (primTag @t undefined) (fromIntegral $ dimVal' @channels )
    let ptr' = unsafeCoerce dataPtr :: Ptr ()
    rPtr <- [CU.exp| Mat * { new cv::Mat( $(int cdimN), $(int * shapePtr), $(int cvType), $(void * ptr')) } |]
    Mat <$> newForeignPtr rPtr (finMatPtr rPtr >> free dataPtr >> free shapePtr)
  where
    dimN = fromIntegral . dimVal $ order ds
    cdimN = fromIntegral dimN :: C.CInt
    ds' :: Dims ds'
    ds' = case Dim.D @channels of
      D1 -> case unsafeEqTypes @_ @ds @ds' of Dict -> ds
      d  -> case unsafeEqTypes @_ @(ds +: channels) @ds' of Dict -> Snoc ds d
    --  Free Mat memory
    finMatPtr :: Ptr C'Mat -> IO ()
    finMatPtr rPtr = [CU.exp| void { delete $(Mat * rPtr) }|]



-- | This one is unsafe in the sense that it does not check the content of the
--   pointer -- just reads the whole thing into a frame.
readMatData :: forall t (xds :: [XNat])
             . ( PrimBytes t
               , KnownXNatTypes xds
               )
            => Dims xds -> Ptr Word8 -> IO (DataFrame t xds)
readMatData (XDims (_ :: Dims ds)) ptr
  | Dict <- inferKnownBackend @t @ds
  = XFrame <$> (peek (unsafeCoerce ptr) :: IO (DataFrame t ds))


-- | Also unsafe in that it does not respect the relation between xds
--   and shape and channels.
--   However, it is guaranteed to fail at runtime if the runtime shape information
--   does not match the dimensionality constraints.
withMatData' :: forall (k :: Type) (xds :: [k]) (b :: Type)
                (shape :: DS [DS Nat]) (channels :: DS Nat) (depth :: DS Type)
              . BoundedDims xds
             => Mat shape channels depth -> (Dims xds -> Ptr Word8 -> IO b) -> IO b
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



-- | Write dims to a pointer as is.
writeDims :: forall k (xs :: [k]) . Ptr C.CInt -> Dims xs -> IO ()
writeDims = go 0
  where
    go :: forall (ds :: [k]) . Int -> Ptr C.CInt -> Dims ds -> IO ()
    go _ _ U = return ()
    go i p (d :* ds) = pokeElemOff p i (fromIntegral $ dimVal d) >> go (i+1) p ds


-- | Given the number of channels, read @n@ values from a pointer and
--   create a Dims object, optionally adding the channels to the end of
--   the dimension list.
readDims  :: forall k (xds :: [k])
           . BoundedDims xds
          => Int32 -> Ptr Int32 -> Int -> IO (Dims xds)
readDims channels ptr n = do
    xs <- map fromIntegral . withChannels channels <$> peekArray n ptr
    case someDimsVal xs of
      SomeDims xds -> fromMaybe
         (fail $ "OpenCV.Core.Types.Mat.DataFrame.readDims: \
                  \read dimensions do not agree with the specified XDim")
         (return <$> constrainDims @k @xds xds)
  where
    withChannels c xs = if c > 1 then addLast c xs else xs
    addLast :: Int32 -> [Int32] -> [Int32]
    addLast y []     = [y]
    addLast y (x:xs) = x : addLast y xs

unsafeEqTypes :: forall (k :: Type) (a :: k) (b :: k) . Dict (a ~ b)
unsafeEqTypes = unsafeCoerce (Dict @(a ~ a))
