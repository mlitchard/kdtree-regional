module SplitRange (minMaxSplitRange) where

import Control.Monad (liftM)
import Data.Vector.V3
import Data.Vector.Class
import Data.Vector.Fancy
import Data.BoundingBox.B3
import Data.Semigroup
import qualified Data.BoundingBox.Range as R
import Data.Trees.KdTree.Regions.KThree.KThreeTree
import Data.Trees.KdTree.Regions.Class
import Data.Trees.KdTree.Regions.Internal

import Utilities

minMaxSplitRange :: [(Vect BBox3, Int)] -> Bool
minMaxSplitRange = all (== True) . minMaxSplitRange'

minMaxSplitRange' :: [(Vect BBox3, Int)] -> [Bool]
minMaxSplitRange' [] = []
minMaxSplitRange' vects =
  minMaxSplitRange' lefts_x  <>
  minMaxSplitRange' rights_x <>
  minMaxSplitRange' lefts_y  <>
  minMaxSplitRange' rights_y <>
  minMaxSplitRange' lefts_z  <>
  minMaxSplitRange' rights_z <>
  lr_min_test <> lr_max_test

  where
  lr_min_test =
    [ minx_left < minx_right
    , miny_left < miny_right
    , minz_left < minz_right
    ]
  lr_max_test =
    [ maxx_left < maxx_right
    , maxy_left < maxy_right
    , maxz_left < maxz_right
    ]
  boxes = map (toBBox . boxInput) vects
  node_bbox = unions $ map (fst . snd) boxes
  boxes_x = sortedBoxes (X AxisX) boxes
    -- ^ sorted by x-axis
  boxes_y = sortedBoxes (Y AxisY) boxes
    -- ^ sorted by y-axis
  boxes_z = sortedBoxes (Z AxisZ) boxes
    -- ^ sorted by z-axis

  median_index_x = medianIndex boxes_x
  split_x = split boxes_x median_index_x
  (left_range_x,right_range_x) = splitRange rangeX (v3x split_x) node_bbox

  median_index_y = medianIndex boxes_y
  split_y = split boxes_y median_index_y
  (left_range_y,right_range_y) = splitRange rangeY (v3y split_y) node_bbox

  median_index_z = medianIndex boxes_z
  split_z = split boxes_z median_index_z
  (left_range_z,right_range_z) = splitRange rangeZ (v3z split_z) node_bbox

  minx_left  = R.min_point left_range_x
  minx_right = R.min_point right_range_x

  miny_left  = R.min_point left_range_y
  miny_right = R.min_point right_range_y

  minz_left  = R.min_point left_range_z
  minz_right = R.min_point right_range_z

  maxx_left  = R.max_point left_range_x
  maxx_right = R.max_point right_range_x

  maxy_left  = R.max_point left_range_y
  maxy_right = R.max_point right_range_y

  maxz_left  = R.max_point left_range_z
  maxz_right = R.max_point right_range_z
  lefts_x',rights_x',lefts_y',rights_y',lefts_z',rights_z' :: [(Vect BBox3,(BBox3,Int))]

  (lefts_x',rights_x') = case splitAt (median_index_x - 1) boxes_x of
                           (lefts, _ : rights@(_:_)) -> (lefts,rights)
                           (lefts,[right])    -> (lefts,[right])
                           (lefts,[])         -> (lefts,[])
  (lefts_y',rights_y') = case splitAt (median_index_y - 1) boxes_y of
                               (lefts, _ : rights@(_:_) ) -> (lefts,rights)
                               (lefts,[right])    -> (lefts,[right])
                               (lefts,[])         -> (lefts,[])
  (lefts_z',rights_z') = case splitAt (median_index_z - 1) boxes_z of
                               (lefts, _ : rights@(_:_) ) -> (lefts,rights)
                               (lefts,[right])    -> (lefts,[right])
                               (lefts,[])         -> (lefts,[])

  lefts_x = map mungeBoxList lefts_x'
  lefts_y = map mungeBoxList lefts_y'
  lefts_z = map mungeBoxList lefts_z'

  rights_x = map mungeBoxList rights_x'
  rights_y = map mungeBoxList rights_y'
  rights_z = map mungeBoxList rights_z'

