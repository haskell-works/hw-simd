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

import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STK
import qualified HaskellWorks.Hedgehog.Gen               as G
import qualified Hedgehog.Gen                            as G
import qualified Hedgehog.Range                          as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

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
