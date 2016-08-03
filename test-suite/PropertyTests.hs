{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module PropertyTests (nonNegative) where

import Test.QuickCheck.Arbitrary hiding ((><))
import Test.SmallCheck.Series
import GHC.Generics

import Control.Monad (liftM)
import Data.Vector.V3
import Data.Vector.Class
import Data.BoundingBox.B3

import Data.Trees.KdTree.Regions.KThree.KThreeTree
import Data.Trees.KdTree.Regions.Class
import Data.Trees.KdTree.Regions.Internal

nonNegative :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
nonNegative (v1,v2) =
  all (>= 0) $ map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
  where
    (b1,_) = toBBox (boxInput v1)
    (b2,_) = toBBox (boxInput v2)


boxInput :: (Vect BBox3,Int) -> (Vect BBox3, BBoxOffset, Int)
boxInput (bbox3,a) = (bbox3,offset,a)

offset :: BBoxOffset
offset = 5.0

instance Generic Vector3
instance Monad m => Serial m Vector3 where
  series = cons3 Vector3 

instance Arbitrary Vector3 where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Vector3 x y z  
