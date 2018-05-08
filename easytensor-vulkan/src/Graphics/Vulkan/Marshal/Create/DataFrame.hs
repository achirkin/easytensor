{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-#OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Vulkan.Marshal.Create.DataFrame where


import           Foreign.Storable
import           GHC.Base                         (Int (..), byteArrayContents#,
                                                   minusAddr#, (+#))
import           Graphics.Vulkan
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Internal
import           Numeric.DataFrame
import           Numeric.PrimBytes



setVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes (Vector t (FieldArrayLength fname x))
          , CanWriteFieldArray fname 0 x
          )
       => Vector t (FieldArrayLength fname x) -> CreateVkStruct x '[fname] ()
setVec v = unsafeIOCreate $ \p -> pokeByteOff p (fieldOffset @fname @x) v


getVec :: forall fname x t
        . ( FieldType fname x ~ t
          , PrimBytes (Vector t (FieldArrayLength fname x))
          , CanReadFieldArray fname 0 x
          , VulkanMarshalPrim x
          )
       => x -> Vector t (FieldArrayLength fname x)
getVec x
  | ba <- unsafeByteArray x
  , xaddr <- unsafeAddr x
  , baddr <- byteArrayContents# ba
  , I# off <- fieldOffset @fname @x
  = fromBytes (minusAddr# xaddr baddr +# off) ba


instance (VulkanMarshalPrim a, Storable a) => PrimBytes a where
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
    readBytes mba off s = undefined
    writeBytes mba off a s = undefined
    readAddr addr s = undefined
    writeAddr a addr s = undefined
