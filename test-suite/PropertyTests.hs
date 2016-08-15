{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module PropertyTests (nonNegative,symmetry,triangularity, minLeftlessThanMinRight) where

import Test.QuickCheck.Arbitrary hiding ((><))
import Test.SmallCheck.Series
import GHC.Generics

import Control.Monad (liftM)
import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy
import Data.BoundingBox.B3
import Data.Semigroup
-- import Data.BoundingBox.Fancy
import qualified Data.BoundingBox.Range as R
import Data.Trees.KdTree.Regions.KThree.KThreeTree
import Data.Trees.KdTree.Regions.Class
import Data.Trees.KdTree.Regions.Internal


-- | tests for boxAxisDistance
-- | nonNegative
--   
nonNegative :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
nonNegative (p,q) =
  all (>= 0) $ map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
  where
    (b1,_) = snd (toBBox (boxInput p))
    (b2,_) = snd (toBBox (boxInput q))

symmetry :: ((Vect BBox3,Int),(Vect BBox3,Int)) -> Bool
symmetry (p,q) =
  notElem False $ map symmetric_test (zip dist1 dist2)
  where
    (b1,_) = snd (toBBox (boxInput p))
    (b2,_) = snd (toBBox (boxInput q))
    dist1 = map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
    dist2 = map (boxAxisDistance b2 b1) [rangeX,rangeY,rangeZ]

triangularity :: ((Vect BBox3,Int),(Vect BBox3,Int), (Vect BBox3,Int)) -> Bool
triangularity (p,q,r) =
  notElem False $ map triangularity_test (zip3 dist_pr dist_pq dist_qr)
  where
     (bp,_) = snd (toBBox (boxInput p))
     (bq,_) = snd (toBBox (boxInput q))
     (br,_) = snd (toBBox (boxInput r))
     dist_pr = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_pq = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_qr = map (boxAxisDistance bq br) [rangeX,rangeY,rangeZ]

------------------------
-- | tests for splitRange
--   
minLeftlessThanMinRight :: [(Vect BBox3, Int)] -> Bool
minLeftlessThanMinRight = all (== True) . minLeftlessThanMinRight' 

minLeftlessThanMinRight' :: [(Vect BBox3, Int)] -> [Bool]
minLeftlessThanMinRight' [] = []
minLeftlessThanMinRight' vects = 
  minLeftlessThanMinRight' lefts_x  <>
  minLeftlessThanMinRight' rights_x <>
  minLeftlessThanMinRight' lefts_y  <>
  minLeftlessThanMinRight' rights_y <>
  minLeftlessThanMinRight' lefts_z  <>
  minLeftlessThanMinRight' rights_z <>
  lr_test
  
  where
  lr_test = 
    [ (minx_left < minx_right)
    , (miny_left < miny_right)
    , (minz_left < minz_right)
    ]
  
  boxes = map toBBox $ map boxInput vects
  node_bbox = unions $ map (fst . snd) boxes
  boxes_x = sortedBoxes (X AxisX) boxes
    -- ^ sorted by x-axis
  boxes_y = sortedBoxes (Y AxisY) boxes
    -- ^ sorted by y-axis
  boxes_z = sortedBoxes (Z AxisZ) boxes
    -- ^ sorted by z-axis
    --
  median_index_x = medianIndex boxes_x
  split_x = fst (split boxes_x median_index_x)
  (left_range_x,right_range_x) = splitRange rangeX (v3x split_x) node_bbox
    
  median_index_y = medianIndex boxes_y
  split_y = fst (split boxes_y median_index_y)
  (left_range_y,right_range_y) = splitRange rangeY (v3y split_y) node_bbox

  median_index_z = medianIndex boxes_z
  split_z = fst (split boxes_z median_index_z)
  (left_range_z,right_range_z) = splitRange rangeZ (v3z split_z) node_bbox

  minx_left = R.min_point left_range_x
  minx_right = R.min_point right_range_x

  miny_left  = R.min_point left_range_y
  miny_right = R.min_point right_range_y

  minz_left  = R.min_point left_range_z
  minz_right = R.min_point right_range_z

  lefts_x',rights_x',lefts_y',rights_y',lefts_z',rights_z' :: [(Vect BBox3,(BBox3,Int))]
  
  (lefts_x',rights_x') = case (splitAt (median_index_x - 1) boxes_x) of
                           (lefts,(_:rights)) -> (lefts,rights)
                           (lefts,[right])    -> (lefts,[right])
                           (lefts,[])         -> (lefts,[])
  (lefts_y',rights_y') = case (splitAt (median_index_y - 1) boxes_y) of
                               (lefts,(_:rights)) -> (lefts,rights)
                               (lefts,[right])    -> (lefts,[right])
                               (lefts,[])         -> (lefts,[]) 
  (lefts_z',rights_z') = case (splitAt (median_index_z - 1) boxes_z) of
                               (lefts,(_:rights)) -> (lefts,rights)
                               (lefts,[right])    -> (lefts,[right])
                               (lefts,[])         -> (lefts,[])

  lefts_x = map mungeBoxList lefts_x'
  lefts_y = map mungeBoxList lefts_y'
  lefts_z = map mungeBoxList lefts_z'

  rights_x = map mungeBoxList rights_x'
  rights_y = map mungeBoxList rights_y'
  rights_z = map mungeBoxList rights_z'

mungeBoxList :: (Vect BBox3, (BBox3, Int)) -> (Vect BBox3, Int)
mungeBoxList (vect,(_,val)) = (vect,val)
-----------
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
