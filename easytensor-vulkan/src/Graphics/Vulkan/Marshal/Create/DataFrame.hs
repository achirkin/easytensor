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
  ) where


import Foreign.Storable
import GHC.Exts                         (unsafeCoerce#)
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
          )
       => x -> Vector t (FieldArrayLength fname x)
getVec x
  | ba <- unsafeByteArray x
  , xaddr <- unsafeAddr x
  , baddr <- byteArrayContents# ba
  , I# off <- fieldOffset @fname @x
  , Dict <- inferKnownBackend @t @'[FieldArrayLength fname x]
  = fromBytes (minusAddr# xaddr baddr +# off) ba

instance VulkanMarshal (VkStruct a) => PrimBytes (VkStruct a) where
    type PrimFields (VkStruct a) = '[]
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
    readBytes mba off = unsafeCoerce# (newVkData @(VkStruct a) f)
      where
        f :: Ptr (VkStruct a) -> IO ()
        f (Ptr addr) = IO $ \s ->
          (# copyMutableByteArrayToAddr# (unsafeCoerce# mba)
                 off addr (byteSize @(VkStruct a) undefined) s
           , () #)
    writeBytes mba off a
      = copyAddrToByteArray# (unsafeAddr a) mba off (byteSize @(VkStruct a) undefined)
    readAddr addr = unsafeCoerce# (peek (Ptr addr) :: IO (VkStruct a))
    writeAddr a addr s
      = case unsafeCoerce# (poke (Ptr addr) a :: IO ()) s of
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
withDFPtr :: forall (a :: Type) (ds :: [Nat]) (b :: Type)
           . (PrimBytes a, Dimensions ds)
          => DataFrame a ds -> (Ptr a -> IO b) -> IO b
withDFPtr x k
  | Dict <- inferKnownBackend @a @ds
  , ba <- getBytesPinned x = do
    b <- k (Ptr (byteArrayContents# ba `plusAddr#` byteOffset x))
    IO $ \s -> (# touch# ba s, () #)
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
setDFRef :: forall fname x a (ds :: [Nat])
          . ( CanWriteField fname x
            , FieldType fname x ~ Ptr a
            , PrimBytes a, Dimensions ds
            )
         => DataFrame a ds -> CreateVkStruct x '[fname] ()
setDFRef v
    | Dict <- inferKnownBackend @a @ds
    , ba <- getBytesPinned v
    , addr <- byteArrayContents# ba `plusAddr#` byteOffset v
     = let f :: Ptr x -> IO ( ([Ptr ()],[IO ()]) , ())
           f p = (,) ([],[IO $ \s -> (# touch# ba s, () #)])
             <$> writeField @fname @x p (Ptr addr)
       in  unsafeCoerce# f -- workaround for the hidden CreateVkStruct constr.


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
  | Dx (D :: Dim n) <- someDimVal n
  , Dict <- inferKnownBackend @a @'[n]
  = do
     mdf <- newPinnedDataFrame
     withDataFramePtr mdf k
     XFrame <$> unsafeFreezeDataFrame @a @'[n] mdf
fillDataFrame _ _ = error "fillDataFrame: impossible combination of arguments."
