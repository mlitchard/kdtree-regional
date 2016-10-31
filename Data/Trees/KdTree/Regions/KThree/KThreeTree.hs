{- |
   Module     : Data.Trees.KdTree.Regions.KThree.KThreeTree
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3

   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable

   This module implements an adaptation of kd-tree for regions.
   http://en.wikipedia.org/wiki/K-d_tree

-}
{-# LANGUAGE InstanceSigs, TypeFamilies #-}
module Data.Trees.KdTree.Regions.KThree.KThreeTree
  ( KdTreeRegional (..)
  , KdTree         (..)
  , Leaf           (..)
  , Axes           (..)
  , sortedBoxes
  , split
  , medianIndex
  , splitBox
  )  where

import Data.Maybe
import Data.Bool
import Data.Vector.V3
import Data.Vector.Class
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Vector.Fancy
import qualified Data.BoundingBox.Range as R
import Data.BoundingBox
import Data.BoundingBox.B3 hiding ( min_point
                                  , max_point
                                  , min_point
                                  , bound_corners
                                  , isect)
import Data.Either
import Data.Semigroup
import Data.Trees.KdTree.Regions.Internal
import Data.Trees.KdTree.Regions.Class

leafSize :: Int
leafSize = 18

instance KdTreeRegional BBox3 where

  type Vect BBox3 = Vector3
  data Axes BBox3 = X AxisX | Y AxisY | Z AxisZ deriving Show

  toBBox :: (Vect BBox3,BBoxOffset,a) -> (Vect BBox3,(BBox3,a))
  toBBox (mp@(Vector3 x y z),offset,a) = (mp,(bbox3,a)) 
    where
      bbox3 = bound_corners nwu sed
      nwu   = Vector3 (x - offset) (y + offset) (z + offset)
      sed   = Vector3 (x + offset) (y - offset) (z - offset)

-- | sortedMP sorts by midpoints based on Axis
sortedBoxes :: Axes BBox3               ->
               [(Vect BBox3,(BBox3,a))] ->
               [(Vect BBox3,(BBox3,a))]
sortedBoxes axis = L.sortBy (sortByAttrib axis)

sortByAttrib :: Axes BBox3             ->
                  (Vect BBox3,(BBox3,a)) ->
                  (Vect BBox3,(BBox3,a)) ->
                  Ordering
sortByAttrib axis p q =
  attribValue axis (fst p) `compare` attribValue axis (fst q)

split :: [(Vect BBox3,(BBox3,a))] -> Int -> Vect BBox3
split sorted median_index = fst (sorted !! median_index)

medianIndex :: [(Vect BBox3,(BBox3,a))] -> Int
medianIndex sorted = length sorted `div` 2

attribValue :: Axes BBox3 -> Vect BBox3 -> Scalar
attribValue (X AxisX) vect = get_coord AxisX vect
attribValue (Y AxisY) vect = get_coord AxisY vect
attribValue (Z AxisZ) vect = get_coord AxisZ vect

splitBox :: Vect BBox3 -> Axes BBox3 -> BBox3 -> (BBox3, BBox3)
splitBox split' (X AxisX) node_bbox = (xbox_left, xbox_right)
  where
    xbox_left  = rangeXYZ leftx_range (rangeY node_bbox) (rangeZ node_bbox)
    xbox_right = rangeXYZ rightx_range (rangeY node_bbox) (rangeZ node_bbox)
    (leftx_range, rightx_range)  = splitRange rangeX (v3x split') node_bbox
splitBox split' (Y AxisY) node_bbox = (ybox_left, ybox_right)
  where
    ybox_left  = rangeXYZ (rangeX node_bbox) lefty_range (rangeZ node_bbox)
    ybox_right = rangeXYZ (rangeX node_bbox) righty_range (rangeZ node_bbox)
    (lefty_range, righty_range) = splitRange rangeY (v3y split') node_bbox
splitBox split' (Z AxisZ) node_bbox = (zbox_left, zbox_right)
  where
    zbox_left  = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) leftz_range
    zbox_right = rangeXYZ (rangeX node_bbox) (rangeY node_bbox) rightz_range
    (leftz_range, rightz_range) = splitRange rangeZ (v3z split') node_bbox
