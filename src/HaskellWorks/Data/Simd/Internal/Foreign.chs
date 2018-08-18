{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Simd.Internal.Foreign where

import Foreign
import HaskellWorks.Data.Simd.Capabilities

#include "../cbits/simd.h"

type UInt8  = {#type uint8_t#}
type UInt64 = {#type uint64_t#}
type Size = {#type size_t#}

avx2Memcpy :: Ptr UInt8 -> Ptr UInt8 -> Size -> IO ()
avx2Memcpy target source len = requireAvx2 $ do
  {#call unsafe avx2_memcpy as c_build_ibs#} target source len
{-# INLINE avx2Memcpy #-}

avx2Cmpeq8 :: UInt8 -> Ptr UInt8 -> Size -> Ptr UInt8 -> IO ()
avx2Cmpeq8 byte target targetLength source = requireAvx2 $ do
  {#call unsafe avx2_cmpeq8 as c_cmpeq8#} byte target targetLength source
{-# INLINE avx2Cmpeq8 #-}

avx2AndBits :: Ptr UInt8 -> Size -> Ptr UInt8 -> Ptr UInt8 -> IO ()
avx2AndBits target targetLength source_a source_b = requireAvx2 $ do
  {#call unsafe avx2_and_bits as c_avx2_and_bits#} target targetLength source_a source_b
{-# INLINE avx2AndBits #-}

avx2AndNotBits :: Ptr UInt8 -> Size -> Ptr UInt8 -> Ptr UInt8 -> IO ()
avx2AndNotBits target targetLength source_a source_b = requireAvx2 $ do
  {#call unsafe avx2_and_not_bits as c_avx2_and_not_bits#} target targetLength source_a source_b
{-# INLINE avx2AndNotBits #-}

avx2NotBits :: Ptr UInt8 -> Size -> Ptr UInt8 -> IO ()
avx2NotBits target targetLength source = requireAvx2 $ do
  {#call unsafe avx2_not_bits as c_avx2_not_bits#} target targetLength source
{-# INLINE avx2NotBits #-}

avx2OrBits :: Ptr UInt8 -> Size -> Ptr UInt8 -> Ptr UInt8 -> IO ()
avx2OrBits target targetLength source_a source_b = requireAvx2 $ do
  {#call unsafe avx2_or_bits as c_avx2_or_bits#} target targetLength source_a source_b
{-# INLINE avx2OrBits #-}

avx2XorBits :: Ptr UInt8 -> Size -> Ptr UInt8 -> Ptr UInt8 -> IO ()
avx2XorBits target targetLength source_a source_b = requireAvx2 $ do
  {#call unsafe avx2_xor_bits as c_avx2_xor_bits#} target targetLength source_a source_b
{-# INLINE avx2XorBits #-}
