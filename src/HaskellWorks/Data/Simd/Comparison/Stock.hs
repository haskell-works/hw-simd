module HaskellWorks.Data.Simd.Comparison.Stock
  ( cmpeq8s
  ) where

import Data.Monoid                          ((<>))
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Simd.Internal.Bits

import qualified Data.Vector.Storable as DVS

cmpeq8s :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
cmpeq8s w8 v = if disalignment == 0
  then DVS.constructN (DVS.length v) go
  else error $ "Unaligned byte string: " <> show disalignment <> ", vLen: " <> show vLen
  where vLen          = DVS.length v
        disalignment  = vLen - (vLen `div` 8) * 8
        w64           = fromIntegral w8 * 0x0101010101010101 :: Word64
        go :: DVS.Vector Word64 -> Word64
        go u = comp w
          where ui = fromIntegral $ DVS.length u
                w0 = testWord8s ((u !!! (ui + 0)) .^. w64)
                w1 = testWord8s ((u !!! (ui + 1)) .^. w64)
                w2 = testWord8s ((u !!! (ui + 2)) .^. w64)
                w3 = testWord8s ((u !!! (ui + 3)) .^. w64)
                w4 = testWord8s ((u !!! (ui + 4)) .^. w64)
                w5 = testWord8s ((u !!! (ui + 5)) .^. w64)
                w6 = testWord8s ((u !!! (ui + 6)) .^. w64)
                w7 = testWord8s ((u !!! (ui + 7)) .^. w64)
                w   = (w7 .<. 56) .|.
                      (w6 .<. 48) .|.
                      (w5 .<. 40) .|.
                      (w4 .<. 32) .|.
                      (w3 .<. 24) .|.
                      (w2 .<. 16) .|.
                      (w1 .<.  8) .|.
                       w0
{-# INLINE cmpeq8s #-}
