{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Comparison.Avx2 where

import Data.Monoid ((<>))
import Data.Word

import qualified Data.Vector.Storable                    as DVS
import qualified Foreign.ForeignPtr                      as F
import qualified Foreign.Marshal.Unsafe                  as F
import qualified Foreign.Ptr                             as F
import qualified HaskellWorks.Data.Simd.Internal.Foreign as F

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

class CmpEqWord8s a where
  cmpEqWord8s :: Word8 -> a -> a

instance CmpEqWord8s (DVS.Vector Word64) where
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
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}
