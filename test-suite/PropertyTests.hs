module PropertyTests (nonNegative) where

import Test.QuickCheck.Arbitrary

import Data.Vector.V3
import Data.BoundingBox.B3

import Data.Trees.KdTree.Regions.KThree.KThreeTree
import Data.Trees.KdTree.Regions.Class
import Data.Trees.KdTree.Regions.Internal

nonNegative :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
nonNegative (v1,v2) =
  all (<= 0) $ map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
  where
    (b1,_) = toBBox (boxInput v1)
    (b2,_) = toBBox (boxInput v2)


boxInput :: (Vect BBox3,Int) -> (Vect BBox3, BBoxOffset, Int)
boxInput (bbox3,a) = (bbox3,offset,a)

offset :: BBoxOffset
offset = 5.0

instance Arbitrary Vector3 where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Vector3 x y z  
