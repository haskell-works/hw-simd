module Main where

import Control.Monad
import Criterion.Main
import Data.Char
import Data.List
import Data.Word
import HaskellWorks.Data.Vector.AsVector64s

import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.Vector.Storable                             as DVS
import qualified HaskellWorks.Data.Simd.Capabilities              as CAP
import qualified HaskellWorks.Data.Simd.Internal.ByteString       as NONE
import qualified HaskellWorks.Data.Simd.Internal.Simd.Avx2.Vector as AVX2
import qualified System.Directory                                 as IO

runCmpeq8Avx2 :: FilePath -> IO [DVS.Vector Word64]
runCmpeq8Avx2 filePath = do
  bs <- LBS.readFile filePath
  let vs = asVector64s 64 bs
  return $ if CAP.avx2Enabled
    then AVX2.cmpeq8s (fromIntegral (ord '8')) <$> vs
    else []

runCmpeq8None :: FilePath -> IO [DVS.Vector Word64]
runCmpeq8None filePath = do
  bs <- LBS.readFile filePath
  let vs = asVector64s 64 bs
  return $ NONE.cmpeq8s (fromIntegral (ord '8')) <$> vs

benchCmpeq8 :: IO [Benchmark]
benchCmpeq8 = do
  entries <- IO.listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $ mempty
    <> [bench ("hw-simd/cmpeq8/avx2/"  <> file) (nfIO (runCmpeq8Avx2 file))]
    <> [bench ("hw-simd/cmpeq8/none/"  <> file) (nfIO (runCmpeq8None file))]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [benchCmpeq8]
  defaultMain benchmarks
