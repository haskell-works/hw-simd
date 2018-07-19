module Main where

import Control.Monad
import Criterion.Main
import Data.Char
import Data.List
import Data.Semigroup                       ((<>))
import Data.Word
import HaskellWorks.Data.Vector.AsVector64s

import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Simd.Capabilities     as CAP
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified System.Directory                        as IO

runCmpEqWord8sAvx2 :: FilePath -> IO [DVS.Vector Word64]
runCmpEqWord8sAvx2 filePath = do
  bs <- LBS.readFile filePath
  let vs = asVector64s 64 bs
  return $ if CAP.avx2Enabled
    then AVX2.cmpEqWord8s (fromIntegral (ord '8')) <$> vs
    else []

runCmpEqWord8sStock :: FilePath -> IO [DVS.Vector Word64]
runCmpEqWord8sStock filePath = do
  bs <- LBS.readFile filePath
  let vs = asVector64s 64 bs
  return $ STOCK.cmpEqWord8s (fromIntegral (ord '8')) <$> vs

benchcmpEqWord8s :: IO [Benchmark]
benchcmpEqWord8s = do
  entries <- IO.listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $ mempty
    <> [bench ("hw-simd/cmpEqWord8s/avx2/"  <> file) (nfIO (runCmpEqWord8sAvx2  file))]
    <> [bench ("hw-simd/cmpEqWord8s/stock/" <> file) (nfIO (runCmpEqWord8sStock file))]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [benchcmpEqWord8s]
  defaultMain benchmarks
