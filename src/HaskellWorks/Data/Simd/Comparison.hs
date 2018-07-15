{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Data.Simd.Comparison where

import Data.Word
import HaskellWorks.Data.Simd.Capabilities

import qualified Data.Vector.Storable                             as DVS
import qualified HaskellWorks.Data.Simd.Internal.ByteString       as NONE
import qualified HaskellWorks.Data.Simd.Internal.Simd.Avx2.Vector as AVX2

cmpeq8s :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
cmpeq8s w bs = if
  | avx2Enabled -> AVX2.cmpeq8s w bs
  | True        -> NONE.cmpeq8s w bs
