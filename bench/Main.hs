module Main where

import Control.Monad
import Criterion.Main
import Data.Char
import Data.Word
import HaskellWorks.Data.Vector.AsVector64s

import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.List                               as L
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Simd.Capabilities     as CAP
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified HaskellWorks.Data.Simd.Logical.Avx2     as AVX2
import qualified HaskellWorks.Data.Simd.Logical.Stock    as STOCK
import qualified System.Directory                        as IO

runCmpEqWord8sAvx2 :: LBS.ByteString -> IO [DVS.Vector Word64]
runCmpEqWord8sAvx2 bs = do
  let vs = asVector64s 64 bs
  return $ if CAP.avx2Enabled
    then AVX2.cmpEqWord8s (fromIntegral (ord '8')) <$> vs
    else []

runCmpEqWord8sStock :: LBS.ByteString -> IO [DVS.Vector Word64]
runCmpEqWord8sStock bs = do
  let vs = asVector64s 64 bs
  return $ STOCK.cmpEqWord8s (fromIntegral (ord '8')) <$> vs

runAndBitsAvx2 :: LBS.ByteString -> IO [DVS.Vector Word64]
runAndBitsAvx2 bs = do
  let vs = asVector64s 64 bs
  return $ (\v -> AVX2.andBits v v) <$> vs

runAndBitsStock :: LBS.ByteString -> IO [DVS.Vector Word64]
runAndBitsStock bs = do
  let vs = asVector64s 64 bs
  return $ (\v -> STOCK.andBits v v) <$> vs

benchcmpEqWord8s :: IO [Benchmark]
benchcmpEqWord8s = do
  entries <- IO.listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `L.isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $
    [ env (LBS.readFile file) $ \bs -> bgroup file
      [ bench ("hw-simd/cmpEqWord8s/avx2/"  <> file) (nfIO (runCmpEqWord8sAvx2  bs))
      , bench ("hw-simd/cmpEqWord8s/stock/" <> file) (nfIO (runCmpEqWord8sStock bs))
      ]
    ]
  return (join benchmarks)

benchAndBits :: IO [Benchmark]
benchAndBits = do
  entries <- IO.listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `L.isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $
    [ env (LBS.readFile file) $ \bs -> bgroup file
      [ bench ("hw-simd/andBits/avx2/"  <> file) (nfIO (runAndBitsAvx2  bs))
      , bench ("hw-simd/andBits/stock/" <> file) (nfIO (runAndBitsStock bs))
      ]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [benchcmpEqWord8s]
    <> [benchAndBits]
  defaultMain benchmarks
