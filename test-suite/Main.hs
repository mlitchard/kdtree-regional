import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Data.List
import Data.Ord

import BoxAxisDistance
import SplitRange
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties] -- add unit tests 

properties :: TestTree
properties = testGroup "KdTree Property Tests" [qcProps] -- ,scProps]

qcProps = testGroup "Checked by QuickCheck"
  [ QC.testProperty "positive definiteness - boxAxisDistance " nonNegative
  , QC.testProperty "symmetry - boxAxisDistance" symmetry
  , QC.testProperty "trianglularity - boxAxisDistance" triangularity
  , QC.testProperty "min/max splitRange test" minMaxSplitRange
  ]

scProps = testGroup "Checked by SmallCheck"
    [ SC.testProperty "boxAxisDistance is non-negative" $ nonNegative ]

{-
main :: IO ()
main = do
    test <- testSpec "kdtree-regional" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True
-}
