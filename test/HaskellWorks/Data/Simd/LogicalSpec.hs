{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Simd.LogicalSpec (spec) where

import Control.Monad
import HaskellWorks.Data.Simd.Capabilities
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Simd.Logical.Avx2  as AVX
import qualified HaskellWorks.Data.Simd.Logical.Stock as STK
import qualified HaskellWorks.Hedgehog.Gen            as G
import qualified Hedgehog.Gen                         as G
import qualified Hedgehog.Range                       as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Simd.LogicalSpec" $ do
  when avx2Enabled $ do
    describe "xorBits" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        (us, vs) <- forAll $ do
          listLength  <- G.int (R.linear 0 5)
          blockCount  <- G.int (R.linear 0 4)
          blockSize   <- G.int (R.singleton 8)
          G.tuple2
            $ G.list (R.singleton listLength)
            $ G.storableVector (R.singleton (blockCount * blockSize))
            $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.xorBits us vs === STK.xorBits us vs
    describe "orBits" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        (us, vs) <- forAll $ do
          listLength  <- G.int (R.linear 0 5)
          blockCount  <- G.int (R.linear 0 4)
          blockSize   <- G.int (R.singleton 8)
          G.tuple2
            $ G.list (R.singleton listLength)
            $ G.storableVector (R.singleton (blockCount * blockSize))
            $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.orBits us vs === STK.orBits us vs
    describe "andBits" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        (us, vs) <- forAll $ do
          listLength  <- G.int (R.linear 0 5)
          blockCount  <- G.int (R.linear 0 4)
          blockSize   <- G.int (R.singleton 8)
          G.tuple2
            $ G.list (R.singleton listLength)
            $ G.storableVector (R.singleton (blockCount * blockSize))
            $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.andBits us vs === STK.andBits us vs
    describe "andNotBits" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        (us, vs) <- forAll $ do
          listLength  <- G.int (R.linear 0 5)
          blockCount  <- G.int (R.linear 0 4)
          blockSize   <- G.int (R.singleton 8)
          G.tuple2
            $ G.list (R.singleton listLength)
            $ G.storableVector (R.singleton (blockCount * blockSize))
            $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.andNotBits us vs === STK.andNotBits us vs
    describe "notBits" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        vs <- forAll $ do
          listLength  <- G.int (R.linear 0 5)
          blockCount  <- G.int (R.linear 0 4)
          blockSize   <- G.int (R.singleton 8)
          G.list (R.singleton listLength)
            $ G.storableVector (R.singleton (blockCount * blockSize))
            $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.notBits vs === STK.notBits vs
