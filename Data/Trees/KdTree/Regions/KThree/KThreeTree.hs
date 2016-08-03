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

  toBBox :: (Vect BBox3,BBoxOffset,a) -> (BBox3,a)
  toBBox ((Vector3 x y z),offset,a) = (bbox3,a) 
    where
      bbox3 = bound_corners nwu sed
      nwu   = Vector3 (x - offset) (y + offset) (z + offset)
      sed   = Vector3 (x + offset) (y - offset) (z - offset)
