{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Logical.Avx2 where

import Data.Word
import HaskellWorks.Data.Simd.Internal.Marshal

import qualified Data.Vector.Storable                    as DVS
import qualified Foreign.ForeignPtr                      as F
import qualified Foreign.Marshal.Unsafe                  as F
import qualified Foreign.Ptr                             as F
import qualified HaskellWorks.Data.Simd.Internal.Foreign as F

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

class XorBits a where
  xorBits :: a -> a -> a

instance XorBits (DVS.Vector Word64) where
  xorBits a b = F.unsafeLocalState $ do
    let (srcAFptr, srcAOffset, srcALength) = unsafeToElemSizedForeignPtr 64 a
    let (srcBFptr, srcBOffset, srcBLength) = unsafeToElemSizedForeignPtr 64 b
    targetFptr <- F.mallocForeignPtrBytes srcALength
    F.withForeignPtr srcAFptr $ \srcAPtr -> do
      F.withForeignPtr srcBFptr $ \srcBPtr -> do
        F.withForeignPtr targetFptr $ \targetPtr -> do
          _ <- F.avx2XorBits
            (F.castPtr targetPtr)
            (fromIntegral (srcALength `min` srcBLength))
            (F.castPtr srcAPtr `F.plusPtr` srcAOffset)
            (F.castPtr srcBPtr `F.plusPtr` srcBOffset)
          return $ DVS.unsafeCast (DVS.unsafeFromForeignPtr targetFptr 0 srcALength :: DVS.Vector Word8)
  {-# INLINE xorBits #-}

instance XorBits [DVS.Vector Word64] where
  xorBits = zipWith xorBits
  {-# INLINE xorBits #-}

class OrBits a where
  orBits :: a -> a -> a

instance OrBits (DVS.Vector Word64) where
  orBits a b = F.unsafeLocalState $ do
    let (srcAFptr, srcAOffset, srcALength) = unsafeToElemSizedForeignPtr 64 a
    let (srcBFptr, srcBOffset, srcBLength) = unsafeToElemSizedForeignPtr 64 b
    targetFptr <- F.mallocForeignPtrBytes srcALength
    F.withForeignPtr srcAFptr $ \srcAPtr -> do
      F.withForeignPtr srcBFptr $ \srcBPtr -> do
        F.withForeignPtr targetFptr $ \targetPtr -> do
          _ <- F.avx2OrBits
            (F.castPtr targetPtr)
            (fromIntegral (srcALength `min` srcBLength))
            (F.castPtr srcAPtr `F.plusPtr` srcAOffset)
            (F.castPtr srcBPtr `F.plusPtr` srcBOffset)
          return $ DVS.unsafeCast (DVS.unsafeFromForeignPtr targetFptr 0 srcALength :: DVS.Vector Word8)
  {-# INLINE orBits #-}

instance OrBits [DVS.Vector Word64] where
  orBits = zipWith orBits
  {-# INLINE orBits #-}

class AndBits a where
  andBits :: a -> a -> a

instance AndBits (DVS.Vector Word64) where
  andBits a b = F.unsafeLocalState $ do
    let (srcAFptr, srcAOffset, srcALength) = unsafeToElemSizedForeignPtr 64 a
    let (srcBFptr, srcBOffset, srcBLength) = unsafeToElemSizedForeignPtr 64 b
    targetFptr <- F.mallocForeignPtrBytes srcALength
    F.withForeignPtr srcAFptr $ \srcAPtr -> do
      F.withForeignPtr srcBFptr $ \srcBPtr -> do
        F.withForeignPtr targetFptr $ \targetPtr -> do
          _ <- F.avx2AndBits
            (F.castPtr targetPtr)
            (fromIntegral (srcALength `min` srcBLength))
            (F.castPtr srcAPtr `F.plusPtr` srcAOffset)
            (F.castPtr srcBPtr `F.plusPtr` srcBOffset)
          return $ DVS.unsafeCast (DVS.unsafeFromForeignPtr targetFptr 0 srcALength :: DVS.Vector Word8)
  {-# INLINE andBits #-}

instance AndBits [DVS.Vector Word64] where
  andBits = zipWith andBits
  {-# INLINE andBits #-}

class AndNotBits a where
  andNotBits :: a -> a -> a

instance AndNotBits (DVS.Vector Word64) where
  andNotBits a b = F.unsafeLocalState $ do
    let (srcAFptr, srcAOffset, srcALength) = unsafeToElemSizedForeignPtr 64 a
    let (srcBFptr, srcBOffset, srcBLength) = unsafeToElemSizedForeignPtr 64 b
    targetFptr <- F.mallocForeignPtrBytes srcALength
    F.withForeignPtr srcAFptr $ \srcAPtr -> do
      F.withForeignPtr srcBFptr $ \srcBPtr -> do
        F.withForeignPtr targetFptr $ \targetPtr -> do
          _ <- F.avx2AndNotBits
            (F.castPtr targetPtr)
            (fromIntegral (srcALength `min` srcBLength))
            (F.castPtr srcAPtr `F.plusPtr` srcAOffset)
            (F.castPtr srcBPtr `F.plusPtr` srcBOffset)
          return $ DVS.unsafeCast (DVS.unsafeFromForeignPtr targetFptr 0 srcALength :: DVS.Vector Word8)
  {-# INLINE andNotBits #-}

instance AndNotBits [DVS.Vector Word64] where
  andNotBits = zipWith andBits
  {-# INLINE andNotBits #-}

class NotBits a where
  notBits :: a -> a

instance NotBits (DVS.Vector Word64) where
  notBits a = F.unsafeLocalState $ do
    let (srcFptr, srcOffset, srcLength) = unsafeToElemSizedForeignPtr 64 a
    targetFptr <- F.mallocForeignPtrBytes srcLength
    F.withForeignPtr srcFptr $ \srcPtr -> do
      F.withForeignPtr targetFptr $ \targetPtr -> do
        _ <- F.avx2NotBits
          (F.castPtr targetPtr)
          (fromIntegral srcLength)
          (F.castPtr srcPtr `F.plusPtr` srcOffset)
        return $ DVS.unsafeCast (DVS.unsafeFromForeignPtr targetFptr 0 srcLength :: DVS.Vector Word8)
  {-# INLINE notBits #-}

instance NotBits [DVS.Vector Word64] where
  notBits = fmap notBits
  {-# INLINE notBits #-}
