module HaskellWorks.Data.Simd.Internal.ByteString.Lazy where

import Data.Word

import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.Simd.Internal.ByteString as BS

toByteString :: [DVS.Vector Word64] -> LBS.ByteString
toByteString vs = LBS.fromChunks $ BS.toByteString <$> vs
