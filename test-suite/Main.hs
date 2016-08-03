import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC
import Data.List
import Data.Ord

import PropertyTests
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties] -- add unit tests 

properties :: TestTree
properties = testGroup "KdTree Property Tests" [qcProps,scProps]

qcProps = testGroup "Checked by QuickCheck"
  [ QC.testProperty "boxAxisDistance is non-negative" $ nonNegative ]

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