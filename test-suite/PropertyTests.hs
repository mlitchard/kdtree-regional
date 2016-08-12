{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module PropertyTests (nonNegative,symmetry,triangularity) where

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

-- | nonNegative
--   
nonNegative :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
nonNegative (p,q) =
  all (>= 0) $ map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
  where
    (b1,_) = toBBox (boxInput p)
    (b2,_) = toBBox (boxInput q)

symmetry :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
symmetry (p,q) =
  notElem False $ map symmetric_test (zip dist1 dist2)
  where
    (b1,_) = toBBox (boxInput p)
    (b2,_) = toBBox (boxInput q)
    dist1 = map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
    dist2 = map (boxAxisDistance b2 b1) [rangeX,rangeY,rangeZ]

triangularity :: ((Vect BBox3,Int),(Vect BBox3,Int), (Vect BBox3,Int)) -> Bool
triangularity (p,q,r) =
  notElem False $ map triangularity_test (zip3 dist_pr dist_pq dist_qr)
  where
     (bp,_) = toBBox (boxInput p)
     (bq,_) = toBBox (boxInput q)
     (br,_) = toBBox (boxInput r)
     dist_pr = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_pq = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_qr = map (boxAxisDistance bq br) [rangeX,rangeY,rangeZ]

symmetric_test :: (Scalar,Scalar) -> Bool
symmetric_test (d1,d2) = d1 == d2

triangularity_test :: (Scalar,Scalar,Scalar) -> Bool
triangularity_test (pr,pq,qr) =
  pr <= pq + qr
  
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
