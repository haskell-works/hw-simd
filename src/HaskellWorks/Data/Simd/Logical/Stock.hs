{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Logical.Stock where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable as DVS

class XorBits a where
  xorBits :: a -> a -> a

instance XorBits (DVS.Vector Word64) where
  xorBits a b = DVS.constructN (DVS.length a `min` DVS.length b) go
    where go v = (a !!! i) .^. (b !!! i)
            where i = end v
  {-# INLINE xorBits #-}

instance XorBits [DVS.Vector Word64] where
  xorBits = zipWith xorBits
  {-# INLINE xorBits #-}

class OrBits a where
  orBits :: a -> a -> a

instance OrBits (DVS.Vector Word64) where
  orBits a b = DVS.constructN (DVS.length a `min` DVS.length b) go
    where go v = (a !!! i) .|. (b !!! i)
            where i = end v
  {-# INLINE orBits #-}

instance OrBits [DVS.Vector Word64] where
  orBits = zipWith orBits
  {-# INLINE orBits #-}

class AndBits a where
  andBits :: a -> a -> a

instance AndBits (DVS.Vector Word64) where
  andBits a b = DVS.constructN (DVS.length a `min` DVS.length b) go
    where go v = (a !!! i) .&. (b !!! i)
            where i = end v
  {-# INLINE andBits #-}

instance AndBits [DVS.Vector Word64] where
  andBits = zipWith andBits
  {-# INLINE andBits #-}

class AndNotBits a where
  andNotBits :: a -> a -> a

instance AndNotBits (DVS.Vector Word64) where
  andNotBits a b = DVS.constructN (DVS.length a `min` DVS.length b) go
    where go v = (a !!! i) .&. comp (b !!! i)
            where i = end v
  {-# INLINE andNotBits #-}

instance AndNotBits [DVS.Vector Word64] where
  andNotBits = zipWith andBits
  {-# INLINE andNotBits #-}

class NotBits a where
  notBits :: a -> a

instance NotBits (DVS.Vector Word64) where
  notBits a = DVS.constructN (DVS.length a) go
    where go v = comp (a !!! i)
            where i = end v
  {-# INLINE notBits #-}

instance NotBits [DVS.Vector Word64] where
  notBits = fmap notBits
  {-# INLINE notBits #-}
