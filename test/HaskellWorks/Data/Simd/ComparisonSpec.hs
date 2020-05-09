{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Simd.ComparisonSpec (spec) where

import Control.Lens
import Control.Monad
import Data.Word
import HaskellWorks.Data.Simd.Capabilities
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector                             as DV
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STK
import qualified HaskellWorks.Hedgehog.Gen               as G
import qualified Hedgehog.Gen                            as G
import qualified Hedgehog.Range                          as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "HaskellWorks.Data.Simd.ComparisonSpec" $ do
  when avx2Enabled $ do
    describe "cmpEqWord8s" $ do
      it "AVX2" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        vs :: [DVS.Vector Word64] <- forAll
          $ G.list (R.linear 0 5)
          $ (\g -> G.list (R.linear 0 4) g <&> join <&> DVS.fromList)
          $ G.list (R.linear 8 8)
          $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        AVX.cmpEqWord8s w vs === STK.cmpEqWord8s w vs
        vs === vs
      it "AVX2 Para 1" $ requireProperty $ do
        w <- forAll $ G.word8 R.constantBounded
        vs :: DVS.Vector Word64 <- forAll
          $ G.storableVector (R.singleton 8)
          $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
        DV.head (AVX.cmpEqWord8sPara (DVS.singleton w) vs) === AVX.cmpEqWord8s w vs
      it "AVX2 Para 2" $ requireProperty $ do
        len <- forAll $ (* 8) <$> G.int (R.linear 0 8)
        w0  <- forAll $ G.word8 R.constantBounded
        w1  <- forAll $ G.word8 R.constantBounded
        vs :: DVS.Vector Word64 <- forAll
          $ G.storableVector (R.singleton len)
          $ G.word8x8 (G.choice [pure w0, pure w1, G.word8 R.constantBounded])
        let actual = AVX.cmpEqWord8sPara (DVS.fromList [w0, w1]) vs
        (actual DV.! 0) === AVX.cmpEqWord8s w0 vs
        (actual DV.! 1) === AVX.cmpEqWord8s w1 vs

