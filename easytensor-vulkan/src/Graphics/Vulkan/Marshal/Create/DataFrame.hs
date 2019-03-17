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
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , VulkanDataFrame (..), VkDataFrame (..)
  ) where


import           Foreign.Storable
import           GHC.Base
import           GHC.Ptr                                 (Ptr (..))
import           Graphics.Vulkan
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Internal
import           Numeric.DataFrame
import           Numeric.DataFrame.Internal.Array.Family (ArraySing (..),
                                                          ArraySingleton (..))
import           Numeric.DataFrame.IO
import           Numeric.Dimensions
import           Numeric.PrimBytes


-- | Write an array of values in one go.
setVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes (Vector t (FieldArrayLength fname x))
          , CanWriteFieldArray fname x
          )
       => Vector t (FieldArrayLength fname x) -> CreateVkStruct x '[fname] ()
setVec v = unsafeIOCreate $ \p -> pokeByteOff p (fieldOffset @fname @x) v

-- | Get an array of values, possibly without copying
--   (if vector implementation allows).
getVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes (Vector t (FieldArrayLength fname x))
          , CanReadFieldArray fname x
          , VulkanMarshalPrim x
          )
       => x -> Vector t (FieldArrayLength fname x)
getVec x
  | ba <- unsafeByteArray x
  , xaddr <- unsafeAddr x
  , baddr <- byteArrayContents# ba
  , I# off <- fieldOffset @fname @x
  = fromBytes (minusAddr# xaddr baddr +# off) ba


instance {-# OVERLAPPABLE#-}
         (VulkanMarshalPrim a, Storable a) => PrimBytes a where
    byteSize   a = case sizeOf a of (I# s) -> s
    {-# INLINE byteSize #-}
    byteAlign  a = case alignment a of (I# n) -> n
    {-# INLINE byteAlign #-}
    byteOffset a = minusAddr# (unsafeAddr a)
                              (byteArrayContents# (unsafeByteArray a))
    {-# INLINE byteOffset #-}
    getBytes = unsafeByteArray
    {-# INLINE getBytes #-}
    fromBytes = unsafeFromByteArrayOffset
    {-# INLINE fromBytes #-}
    readBytes mba off = unsafeCoerce# (newVkData# f)
      where
        f :: Ptr a -> IO ()
        f (Ptr addr) = IO $ \s ->
          (# copyMutableByteArrayToAddr# (unsafeCoerce# mba)
                 off addr (byteSize @a undefined) s
           , () #)
    writeBytes mba off a
      = copyAddrToByteArray# (unsafeAddr a) mba off (byteSize @a undefined)
    readAddr addr = unsafeCoerce# (peekVkData# (Ptr addr) :: IO a)
    writeAddr a addr s
      = case unsafeCoerce# (pokeVkData# (Ptr addr) a :: IO ()) s of
         (# s', () #) -> s'




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
  , E <- inferASing' @a @'[n]
  , E <- inferPrim' @a @'[n]
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


instance (PrimBytes a, ArraySingleton a ds, Dimensions ds)
      => VulkanDataFrame a (ds :: [Nat]) where
    frameToVkData x = case aSing @a @ds of
      -- Generic ByteArray implementation
      ABase
        | ba <- getBytes x
        , isTrue# (isByteArrayPinned# ba) -- check if it is pinned
        -> unsafeFromByteArrayOffset (byteOffset x) ba
      -- For other implementations we have no other options than just to copy the data
      _ | E <- inferPrim x -> case runRW#
        ( \s0 -> case newAlignedPinnedByteArray#
                        (byteSize @(DataFrame a ds) undefined)
                        (byteAlign @(DataFrame a ds) undefined) s0 of
          (# s1, mba #) -> unsafeFreezeByteArray# mba (writeBytes mba 0# x s1)
        ) of (# _, ba #) -> VkDataFrame# (byteArrayContents# ba) ba

    vkDataToFrame _ (VkDataFrame# addr ba)
      | E <- inferPrim' @a @ds
      = fromBytes (addr `minusAddr#` byteArrayContents# ba) ba

instance (PrimBytes a, All KnownXNatType ds)
      => VulkanDataFrame a (ds :: [XNat]) where
    frameToVkData (XFrame x) = unsafeCoerce# (frameToVkData x)
    vkDataToFrame (XDims (ds :: Dims ns)) d
      | E <- inferASing' @a @ns = XFrame (vkDataToFrame ds (unsafeCoerce# d))
