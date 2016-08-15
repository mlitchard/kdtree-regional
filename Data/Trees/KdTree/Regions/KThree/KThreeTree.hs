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
sortedBoxes axis boxes = L.sortBy (sort_by_attrib axis) boxes

sort_by_attrib :: Axes BBox3             ->
                  (Vect BBox3,(BBox3,a)) ->
                  (Vect BBox3,(BBox3,a)) ->
                  Ordering
sort_by_attrib axis p q =
  (attrib_value axis (fst p)) `compare` (attrib_value axis (fst q))

split :: [(Vect BBox3,(BBox3,a))] -> Int -> (Vect BBox3,(BBox3,a))
split sorted median_index = sorted !! median_index

medianIndex :: [(Vect BBox3,(BBox3,a))] -> Int
medianIndex sorted = (length sorted) `div` 2

attrib_value :: Axes BBox3 -> Vect BBox3 -> Scalar
attrib_value (X AxisX) vect = get_coord AxisX vect
attrib_value (Y AxisY) vect = get_coord AxisY vect
attrib_value (Z AxisZ) vect = get_coord AxisZ vect
