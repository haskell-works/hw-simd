{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Data.Simd.Comparison where

import Data.Word
import HaskellWorks.Data.Simd.Capabilities

import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK

cmpeq8s :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
cmpeq8s w bs = if
  | avx2Enabled -> AVX2.cmpeq8s w bs
  | True        -> STOCK.cmpeq8s w bs
