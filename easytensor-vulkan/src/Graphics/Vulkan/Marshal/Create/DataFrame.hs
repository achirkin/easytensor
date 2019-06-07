{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module provides an orphan instance of `PrimBytes` for `VulkanMarshalPrim`
--   structures.
--   This enables them to be stored in @DataFrames@ from @easytensor@ package.
--   Thanks to internal structure of Vulkan structures, they can be manipulated
--   inside DataFrames in a very efficient way (just by copying byte arrays).
--   However, original @DataFrames@ are based on unpinned arrays;
--   functions here check this and copy data to new pinned arrays if needed.
--
--   In addition to the orphan instance, this module provides a few
--   handy helper functions.
module Graphics.Vulkan.Marshal.Create.DataFrame
  ( setVec, getVec
  , fillDataFrame, withDFPtr, setDFRef
    -- * Helpers
  , VulkanDataFrame (..), VkDataFrame (..), VkPrimBytes (..)
  , inferVkPrimBytes
  , Dict (..)
  ) where


import Foreign.Storable
import GHC.Base
import GHC.Ptr                          (Ptr (..))
import Graphics.Vulkan
import Graphics.Vulkan.Marshal.Create
import Graphics.Vulkan.Marshal.Internal
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.PrimBytes


-- | Write an array of values in one go.
setVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes t
          , KnownDim (FieldArrayLength fname x)
          , CanWriteFieldArray fname x
          )
       => Vector t (FieldArrayLength fname x) -> CreateVkStruct x '[fname] ()
setVec v
  | Dict <- inferKnownBackend @t @'[FieldArrayLength fname x]
    = unsafeIOCreate $ \p -> pokeByteOff p (fieldOffset @fname @x) v


-- | Get an array of values, possibly without copying
--   (if vector implementation allows).
getVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes t
          , KnownDim (FieldArrayLength fname x)
          , CanReadFieldArray fname x
          , VulkanMarshalPrim x
          )
       => x -> Vector t (FieldArrayLength fname x)
getVec x
  | ba <- unsafeByteArray x
  , xaddr <- unsafeAddr x
  , baddr <- byteArrayContents# ba
  , I# off <- fieldOffset @fname @x
  , Dict <- inferKnownBackend @t @'[FieldArrayLength fname x]
  = fromBytes (minusAddr# xaddr baddr +# off) ba

-- | Let Vulkan struct behave like PrimBytes!
newtype VkPrimBytes a =  VkPrimBytes { unVkPrimBytes :: a }
  deriving (Eq, Ord, Show, Read)

inferVkPrimBytes :: forall (a :: Type)
                  . (VulkanMarshalPrim a, Storable a)
                 => Dict (PrimBytes a)
inferVkPrimBytes = unsafeCoerce# (Dict @(PrimBytes (VkPrimBytes a)))

instance (VulkanMarshalPrim a, Storable a) => PrimBytes (VkPrimBytes a) where
    type PrimFields (VkPrimBytes a) = '[]
    byteSize   (VkPrimBytes a) = case sizeOf a of (I# s) -> s
    {-# INLINE byteSize #-}
    byteAlign  (VkPrimBytes a) = case alignment a of (I# n) -> n
    {-# INLINE byteAlign #-}
    byteOffset (VkPrimBytes a) = minusAddr# (unsafeAddr a)
                                  (byteArrayContents# (unsafeByteArray a))
    {-# INLINE byteOffset #-}
    getBytes = coerce (unsafeByteArray @a)
    {-# INLINE getBytes #-}
    fromBytes = coerce (unsafeFromByteArrayOffset @a)
    {-# INLINE fromBytes #-}
    readBytes mba off = unsafeCoerce# (newVkData# f)
      where
        f :: Ptr a -> IO ()
        f (Ptr addr) = IO $ \s ->
          (# copyMutableByteArrayToAddr# (unsafeCoerce# mba)
                 off addr (byteSize @(VkPrimBytes a) undefined) s
           , () #)
    writeBytes mba off (VkPrimBytes a)
      = copyAddrToByteArray# (unsafeAddr a) mba off (byteSize @(VkPrimBytes a) undefined)
    readAddr addr = unsafeCoerce# (peekVkData# (Ptr addr) :: IO a)
    writeAddr (VkPrimBytes a) addr s
      = case unsafeCoerce# (pokeVkData# (Ptr addr) a :: IO ()) s of
         (# s', () #) -> s'
    byteFieldOffset _ _ = negateInt# 1#



-- | Run some operation with a pointer to the first item in the frame.
--   All items of the frame are kept in a contiguous memory area accessed by
--   that pointer.
--
--   The function attempts to get an underlying `ByteArray#` without data copy;
--   otherwise, it creates a new pinned `ByteArray#` and passes a pointer to it.
--   Therefore:
--
--     * Sometimes, @Ptr a@ points to the original DF; sometimes, to a copied one.
--     * If the original DF is based on unpinned `ByteArray#`, using this
--       performs a copy anyway.
--
withDFPtr :: VulkanDataFrame a ds
          => DataFrame a ds -> (Ptr a -> IO b) -> IO b
withDFPtr x k
  | d <- frameToVkData x = do
  b <- k (Ptr (unsafeAddr d))
  touchVkData# d
  return b

-- | A variant of `setVkRef` that writes a pointer to a contiguous array of
--   structures.
--
--   Write a pointer to a vulkan structure - member of current structure
--    and make sure the member exists as long as this structure exists.
--
--   Prefer this function to using @unsafePtr a@, because the latter
--    does not keep the dependency information in GC, which results in
--    member structure being garbage-collected and the reference being invalid.
setDFRef :: forall fname x a ds
          . ( CanWriteField fname x
            , FieldType fname x ~ Ptr a
            , VulkanDataFrame a ds
            )
         => DataFrame a ds -> CreateVkStruct x '[fname] ()
setDFRef v = unsafeCoerce# f -- workaround for the hidden CreateVkStruct constr.
  where
    d = frameToVkData v
    f :: Ptr x -> IO ( ([Ptr ()],[IO ()]) , ())
    f p = (,) ([],[touchVkData# d])
       <$> writeField @fname @x p (Ptr (unsafeAddr d))



-- | Given the number of elements, create a new pinned DataFrame and initialize
--   it using the provided function.
--
--   The argument function is called one time with a `Ptr` pointing to the
--   beginning of a contiguous array.
--   This array is converted into a dataframe, possibly without copying.
--
--   It is safe to pass result of this function to `withDFPtr`.
fillDataFrame :: forall a
               . PrimBytes a
              => Word -> (Ptr a -> IO ()) -> IO (Vector a (XN 0))
fillDataFrame n k
  | Dx (_ :: Dim n) <- someDimVal n
  , Dict <- inferKnownBackend @a @'[n]
  = do
     mdf <- newPinnedDataFrame
     withDataFramePtr mdf k
     XFrame <$> unsafeFreezeDataFrame @a @'[n] mdf
fillDataFrame _ _ = error "fillDataFrame: impossible combination of arguments."




-- | Special data type used to provide @VulkanMarshal@ instance for DataFrames.
--   It is guaranteed to be pinned.
data VkDataFrame (t :: l) (ds :: [k]) = VkDataFrame# Addr# ByteArray#

instance PrimBytes (DataFrame t ds) => Storable (VkDataFrame t ds) where
    sizeOf _ = I# (byteSize @(DataFrame t ds) undefined)
    {-# INLINE sizeOf #-}
    alignment _ = I# (byteAlign @(DataFrame t ds) undefined)
    {-# INLINE alignment #-}
    peek = peekVkData#
    {-# INLINE peek #-}
    poke = pokeVkData#
    {-# INLINE poke #-}

instance VulkanMarshalPrim (VkDataFrame t ds) where
    unsafeAddr (VkDataFrame# a _) = a
    {-# INLINE unsafeAddr #-}
    unsafeByteArray (VkDataFrame# _ b) = b
    {-# INLINE unsafeByteArray #-}
    unsafeFromByteArrayOffset off b
      = VkDataFrame# (plusAddr# (byteArrayContents# b) off) b
    {-# INLINE unsafeFromByteArrayOffset #-}

instance PrimBytes (DataFrame t ds) => VulkanMarshal (VkDataFrame t ds) where
    type StructFields (VkDataFrame t ds) = '[]
    type CUnionType (VkDataFrame t ds) = 'False
    type ReturnedOnly (VkDataFrame t ds) = 'False
    type StructExtends (VkDataFrame t ds) = '[]


class VulkanDataFrame a (ds :: [k]) where
    -- | Construct a new @VkDataFrame@ possibly without copying.
    --   It performs no copy if the @DataFrame@ implementation is a pinned @ByteArray#@.
    frameToVkData :: DataFrame a ds -> VkDataFrame a ds
    -- | Construct a new (pinned if implementation allows) DataFrame from VK data,
    --   possibly without copying.
    --
    --   Note, this is a user responsibility to check if the real size of @VkDataFrame@
    --   and the dimensionality @ds@ agree.
    vkDataToFrame :: Dims ds -> VkDataFrame a ds -> DataFrame a ds


instance (PrimBytes a, Dimensions ds)
      => VulkanDataFrame a (ds :: [Nat]) where
    frameToVkData x
      | Dict <- inferKnownBackend @a @ds
        = unsafeFromByteArrayOffset (byteOffset x) (getBytesPinned x)
    vkDataToFrame _ (VkDataFrame# addr ba)
      | Dict <- inferKnownBackend @a @ds
        = fromBytes (addr `minusAddr#` byteArrayContents# ba) ba

instance (PrimBytes a, All KnownXNatType ds)
      => VulkanDataFrame a (ds :: [XNat]) where
    frameToVkData (XFrame x) = unsafeCoerce# (frameToVkData x)
    vkDataToFrame (XDims (ds :: Dims ns)) d
      | Dict <- inferKnownBackend @a @ns
        = XFrame (vkDataToFrame ds (unsafeCoerce# d))
