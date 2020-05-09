{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Simd.Internal.Marshal where

import Data.Word

import qualified Data.Vector.Storable as DVS
import qualified Foreign.ForeignPtr   as F

{- HLINT ignore "Redundant do"        -}

unsafeToElemSizedForeignPtr :: Int -> DVS.Vector Word64 -> (F.ForeignPtr Word8, Int, Int)
unsafeToElemSizedForeignPtr elemSize v = case DVS.unsafeCast v :: DVS.Vector Word8 of
  au -> case DVS.unsafeToForeignPtr au of
    t@(_, _, srcALength) -> if sizeMismatch == 0
      then t
      else error $ "Byte string with mismatched element size: " <> show sizeMismatch
      where w64sLen       = srcALength `div` elemSize
            sizeMismatch  = srcALength - w64sLen * elemSize
