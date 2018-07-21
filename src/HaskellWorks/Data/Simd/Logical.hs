{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Simd.Logical
  ( XorBits(..)
  , OrBits(..)
  , AndBits(..)
  , AndNotBits(..)
  , NotBits(..)
  ) where

import Data.Word
import HaskellWorks.Data.Simd.Capabilities

import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.Simd.Logical.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Logical.Stock as STOCK

class XorBits a where
  xorBits :: a -> a -> a

instance XorBits (DVS.Vector Word64) where
  xorBits a b = if
    | avx2Enabled -> AVX2.xorBits  a b
    | True        -> STOCK.xorBits a b
  {-# INLINE xorBits #-}

class OrBits a where
  orBits :: a -> a -> a

instance OrBits (DVS.Vector Word64) where
  orBits a b = if
    | avx2Enabled -> AVX2.orBits  a b
    | True        -> STOCK.orBits a b
  {-# INLINE orBits #-}

class AndBits a where
  andBits :: a -> a -> a

instance AndBits (DVS.Vector Word64) where
  andBits a b = if
    | avx2Enabled -> AVX2.andBits  a b
    | True        -> STOCK.andBits a b
  {-# INLINE andBits #-}

class AndNotBits a where
  andNotBits :: a -> a -> a

instance AndNotBits (DVS.Vector Word64) where
  andNotBits a b = if
    | avx2Enabled -> AVX2.andNotBits  a b
    | True        -> STOCK.andNotBits a b
  {-# INLINE andNotBits #-}

class NotBits a where
  notBits :: a -> a

instance NotBits (DVS.Vector Word64) where
  notBits a = if
    | avx2Enabled -> AVX2.notBits  a
    | True        -> STOCK.notBits a
  {-# INLINE notBits #-}

