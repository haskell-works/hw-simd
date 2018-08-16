{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Comparison.Stock
  ( CmpEqWord8s(..)
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Simd.Internal.Bits
import HaskellWorks.Data.Simd.Internal.Broadword

import qualified Data.ByteString                    as BS
import qualified Data.Vector.Storable               as DVS
import qualified HaskellWorks.Data.ByteString       as BS
import qualified HaskellWorks.Data.Simd.ChunkString as CS
import qualified HaskellWorks.Data.Vector.AsVector8 as V

class CmpEqWord8s a where
  cmpEqWord8s :: Word8 -> a -> a

instance CmpEqWord8s (DVS.Vector Word8) where
  cmpEqWord8s w8 v = DVS.unsafeCast (cmpEqWord8s w8 (DVS.unsafeCast v :: DVS.Vector Word64))

instance CmpEqWord8s (DVS.Vector Word64) where
  cmpEqWord8s w8 v = DVS.constructN ((DVS.length v + 7) `div` 8) go
    where iw = fillWord64 w8
          go :: DVS.Vector Word64 -> Word64
          go u = let ui = end u in
            if ui * 8 + 8 < end v
              then  let vi  = ui * 8
                        w0  = testWord8s ((v !!! (vi + 0)) .^. iw)
                        w1  = testWord8s ((v !!! (vi + 1)) .^. iw)
                        w2  = testWord8s ((v !!! (vi + 2)) .^. iw)
                        w3  = testWord8s ((v !!! (vi + 3)) .^. iw)
                        w4  = testWord8s ((v !!! (vi + 4)) .^. iw)
                        w5  = testWord8s ((v !!! (vi + 5)) .^. iw)
                        w6  = testWord8s ((v !!! (vi + 6)) .^. iw)
                        w7  = testWord8s ((v !!! (vi + 7)) .^. iw)
                        w   = (w7 .<. 56) .|.
                              (w6 .<. 48) .|.
                              (w5 .<. 40) .|.
                              (w4 .<. 32) .|.
                              (w3 .<. 24) .|.
                              (w2 .<. 16) .|.
                              (w1 .<.  8) .|.
                                w0
                    in comp w
              else  let vi  = ui * 8
                        w0  = testWord8s (atIndexOr 0 v (vi + 0) .^. iw)
                        w1  = testWord8s (atIndexOr 0 v (vi + 1) .^. iw)
                        w2  = testWord8s (atIndexOr 0 v (vi + 2) .^. iw)
                        w3  = testWord8s (atIndexOr 0 v (vi + 3) .^. iw)
                        w4  = testWord8s (atIndexOr 0 v (vi + 4) .^. iw)
                        w5  = testWord8s (atIndexOr 0 v (vi + 5) .^. iw)
                        w6  = testWord8s (atIndexOr 0 v (vi + 6) .^. iw)
                        w7  = testWord8s (atIndexOr 0 v (vi + 7) .^. iw)
                        w   = (w7 .<. 56) .|.
                              (w6 .<. 48) .|.
                              (w5 .<. 40) .|.
                              (w4 .<. 32) .|.
                              (w3 .<. 24) .|.
                              (w2 .<. 16) .|.
                              (w1 .<.  8) .|.
                              w0
                    in comp w
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word64] where
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word8] where
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [BS.ByteString] where
  cmpEqWord8s w8 vs = BS.toByteString . cmpEqWord8s w8 . V.asVector8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s CS.ChunkString where
  cmpEqWord8s w8 = CS.toChunkString . cmpEqWord8s w8 . BS.toByteStrings
  {-# INLINE cmpEqWord8s #-}
