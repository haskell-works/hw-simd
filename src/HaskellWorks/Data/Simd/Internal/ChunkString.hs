{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Internal.ChunkString where

import Data.Word
import HaskellWorks.Data.ByteString
import HaskellWorks.Data.ByteString.Lazy

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.ByteString as BS

newtype ChunkString = ChunkString
  { chunks :: [BS.ByteString]
  } deriving (Eq, Show)

instance ToLazyByteString ChunkString where
  toLazyByteString (ChunkString cs) = LBS.fromChunks cs

instance ToByteStrings ChunkString where
  toByteStrings (ChunkString bss) = bss

class ToChunkString a where
  toChunkString :: a -> ChunkString

instance ToChunkString LBS.ByteString where
  toChunkString = toChunkString . LBS.toChunks

instance ToChunkString [BS.ByteString] where
  toChunkString = ChunkString . BS.rechunk chunkSize

instance ToChunkString [DVS.Vector Word8] where
  toChunkString = toChunkString . (toByteString <$>)

instance ToChunkString [DVS.Vector Word16] where
  toChunkString = toChunkString . (toByteString <$>)

instance ToChunkString [DVS.Vector Word32] where
  toChunkString = toChunkString . (toByteString <$>)

instance ToChunkString [DVS.Vector Word64] where
  toChunkString = toChunkString . (toByteString <$>)

instance ToChunkString (DVS.Vector Word8) where
  toChunkString v = toChunkString [toByteString v]

instance ToChunkString (DVS.Vector Word16) where
  toChunkString v = toChunkString [toByteString v]

instance ToChunkString (DVS.Vector Word32) where
  toChunkString v = toChunkString [toByteString v]

instance ToChunkString (DVS.Vector Word64) where
  toChunkString v = toChunkString [toByteString v]

#if 0
instance BitWise ChunkString where
  ChunkString (a:as) .&. ChunkString (b:bs) = ChunkString (c:cs)
    where len             = end a `min` end b
          c               = toByteString $ DVS.constructN (fromIntegral len) go
          ChunkString cs  = ChunkString as .&. ChunkString bs
          go u            = let ui = end u in (a !!! ui) .&. (b !!! ui)
  ChunkString _ .&. ChunkString _ = ChunkString []
  {-# INLINE (.&.) #-}

  ChunkString (a:as) .|. ChunkString (b:bs) = ChunkString (c:cs)
    where len             = end a `min` end b
          c               = toByteString $ DVS.constructN (fromIntegral len) go
          ChunkString cs  = ChunkString as .|. ChunkString bs
          go u            = let ui = end u in (a !!! ui) .&. (b !!! ui)
  ChunkString _ .|. ChunkString _ = ChunkString []
  {-# INLINE (.|.) #-}

  ChunkString (a:as) .^. ChunkString (b:bs) = ChunkString (c:cs)
    where len             = end a `min` end b
          c               = toByteString $ DVS.constructN (fromIntegral len) go
          ChunkString cs  = ChunkString as .^. ChunkString bs
          go u            = let ui = end u in (a !!! ui) .&. (b !!! ui)
  ChunkString _ .^. ChunkString _ = ChunkString []
  {-# INLINE (.^.) #-}

  comp (ChunkString (a:as)) = ChunkString (c:cs)
    where c               = toByteString $ DVS.constructN (fromIntegral (end a)) go
          ChunkString cs  = comp (ChunkString as)
          go u            = let ui = end u in comp (a !!! ui)
  comp (ChunkString _) = ChunkString []
  {-# INLINE comp #-}

  all0s = ChunkString $ repeat $ toByteString $ DVS.replicate chunkSize (0x00 :: Word8)
  {-# INLINE all0s #-}

  all1s = ChunkString $ repeat $ toByteString $ DVS.replicate chunkSize (0xff :: Word8)
  {-# INLINE all1s #-}
#endif

chunkSize :: Int
chunkSize = 32 * 1024 - 64
{-# INLINE chunkSize #-}
