{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Utilities (boxInput,offset,mungeBoxList) where
import Test.QuickCheck.Arbitrary hiding ((><))
import Test.SmallCheck.Series
import GHC.Generics

import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy
import Data.BoundingBox.B3
import Data.Semigroup
import qualified Data.BoundingBox.Range as R
import Data.Trees.KdTree.Regions.KThree.KThreeTree
import Data.Trees.KdTree.Regions.Class
import Data.Trees.KdTree.Regions.Internal

boxInput :: (Vect BBox3,Int) -> (Vect BBox3, BBoxOffset, Int)
boxInput (vect,a) = (vect,offset,a)

offset :: BBoxOffset
offset = 5.0

mungeBoxList :: (Vect BBox3, (BBox3, Int)) -> (Vect BBox3, Int)
mungeBoxList (vect,(_,val)) = (vect,val)

instance Generic Vector3
instance Monad m => Serial m Vector3 where
  series = cons3 Vector3

instance Arbitrary Vector3 where
  arbitrary = do x <- arbitrary
                 y <- arbitrary
                 z <- arbitrary
                 return $ Vector3 x y z

