{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HaskellWorks.Data.Simd.Comparison.Avx2 where

import Control.Monad
import Data.Word

import qualified Data.ByteString                         as BS
import qualified Data.Vector                             as DV
import qualified Data.Vector.Storable                    as DVS
import qualified Foreign.ForeignPtr                      as F
import qualified Foreign.Marshal.Unsafe                  as F
import qualified Foreign.Ptr                             as F
import qualified HaskellWorks.Data.ByteString            as BS
import qualified HaskellWorks.Data.Simd.ChunkString      as CS
import qualified HaskellWorks.Data.Simd.Internal.Foreign as F
import qualified HaskellWorks.Data.Vector.AsVector8      as V
import qualified HaskellWorks.Data.Vector.Storable       as DVS

{- HLINT ignore "Redundant do"        -}

class CmpEqWord8s a where
  type Target a
  cmpEqWord8s :: Word8 -> a -> Target a

instance CmpEqWord8s (DVS.Vector Word8) where
  type Target (DVS.Vector Word8) = DVS.Vector Word8
  cmpEqWord8s w8 v = F.unsafeLocalState $ do
    tgtFptr <- F.mallocForeignPtrBytes bufLen
    F.withForeignPtr srcFptr $ \srcPtr -> do
      F.withForeignPtr tgtFptr $ \tgtPtr -> do
        _ <- F.avx2Cmpeq8 (fromIntegral w8) (F.castPtr tgtPtr) (fromIntegral w64sLen) (srcPtr `F.plusPtr` srcOffset)
        when (disalignment /= 0) $ do
          let ending = DVS.padded ((DVS.length v + 63) `div` 64)  (DVS.drop alignment v)
          let (endFptr, _, _) = DVS.unsafeToForeignPtr ending
          F.withForeignPtr endFptr $ \endPtr -> do
            void $ F.avx2Cmpeq8 (fromIntegral w8) (tgtPtr `F.plusPtr` (w64sLen * 8)) (fromIntegral disalignment) (F.castPtr endPtr)

        return $ DVS.unsafeFromForeignPtr tgtFptr 0 tgtLen
    where (srcFptr, srcOffset, srcLen) = DVS.unsafeToForeignPtr v
          bufLen        = (srcLen + 63) `div` 8
          tgtLen        = (srcLen + 7) `div` 8
          w64sLen       = srcLen `div` 64
          alignment     = w64sLen * 64
          disalignment  = srcLen - alignment
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s (DVS.Vector Word64) where
  type Target (DVS.Vector Word64) = DVS.Vector Word64
  cmpEqWord8s w8 v = case DVS.unsafeCast v :: DVS.Vector Word8 of
    u -> case DVS.unsafeToForeignPtr u of
      (srcFptr, srcOffset, srcLength) -> if disalignment == 0
        then F.unsafeLocalState $ do
          targetFptr <- F.mallocForeignPtrBytes srcLength
          F.withForeignPtr srcFptr $ \srcPtr -> do
            F.withForeignPtr targetFptr $ \targetPtr -> do
              _ <- F.avx2Cmpeq8
                (fromIntegral w8)
                (F.castPtr targetPtr)
                (fromIntegral w64sLen)
                (F.castPtr srcPtr `F.plusPtr` srcOffset)
              return $ DVS.unsafeFromForeignPtr targetFptr 0 w64sLen
        else error $ "Unaligned byte string: " <> show disalignment
        where w64sLen       = srcLength `div` 64
              disalignment  = srcLength - w64sLen * 64
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word64] where
  type Target [DVS.Vector Word64] = [DVS.Vector Word64]
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word8] where
  type Target [DVS.Vector Word8] = [DVS.Vector Word8]
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [BS.ByteString] where
  type Target [BS.ByteString] = [BS.ByteString]
  cmpEqWord8s w8 vs = BS.toByteString . cmpEqWord8s w8 . V.asVector8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s CS.ChunkString where
  type Target CS.ChunkString = CS.ChunkString
  cmpEqWord8s w8 = CS.toChunkString . cmpEqWord8s w8 . BS.toByteStrings
  {-# INLINE cmpEqWord8s #-}

class CmpEqWord8sPara a where
  type CmpEqWord8sParaTarget a
  cmpEqWord8sPara :: DVS.Vector Word8 -> a -> CmpEqWord8sParaTarget a

instance CmpEqWord8sPara (DVS.Vector Word64) where
  type CmpEqWord8sParaTarget (DVS.Vector Word64) = DV.Vector (DVS.Vector Word64)
  cmpEqWord8sPara w8s v = case DVS.unsafeCast v :: DVS.Vector Word8 of
    u -> case DVS.unsafeToForeignPtr u of
      (srcFptr, srcOffset, srcLength) -> if disalignment == 0
        then F.unsafeLocalState $ do
          tgtFptr <- F.mallocForeignPtrBytes (srcLength * DVS.length w8s)
          F.withForeignPtr srcFptr $ \srcPtr -> do
            F.withForeignPtr tgtFptr $ \tgtPtr -> do
              let tgtsPtrsV :: DVS.Vector (F.Ptr Word8) = DVS.constructN (DVS.length w8s) $ \t ->
                    tgtPtr `F.plusPtr` (DVS.length t * DVS.length v)
              let (w8sFptr, _, w8sLen) = DVS.unsafeToForeignPtr w8s
              let (tgtsPtrsFptr, _, _) = DVS.unsafeToForeignPtr tgtsPtrsV
              F.withForeignPtr w8sFptr $ \w8sPtr -> do
                F.withForeignPtr tgtsPtrsFptr $ \tgtsPtrsPtr -> do
                  F.avx2Cmpeq8Para (F.castPtr w8sPtr) (fromIntegral w8sLen) (F.castPtr tgtsPtrsPtr) (fromIntegral w64sLen) (srcPtr `F.plusPtr` srcOffset)

              let tgtV = DVS.unsafeFromForeignPtr tgtFptr 0 (w64sLen * DVS.length w8s)
              return $ DV.constructN (DVS.length w8s) $ \t -> DVS.take w64sLen (DVS.drop (DV.length t * w64sLen) tgtV)

        else error $ "Unaligned byte string: " <> show disalignment
        where w64sLen       = srcLength `div` 64
              disalignment  = srcLength - w64sLen * 64
  {-# INLINE cmpEqWord8sPara #-}

