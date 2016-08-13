{- |
   Module     : Data.Trees.KdTree.Regions.Class
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3
                       
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable
                                                         
   This module provides a class and general utility functions 
   for kd-trees adapted to regions.

   http://en.wikipedia.org/wiki/K-d_tree 

-}

{-# LANGUAGE TypeFamilies, TypeSynonymInstances, InstanceSigs #-}

module Data.Trees.KdTree.Regions.Class (KdTreeRegional (..), BBoxOffset) where

import           Data.Vector.Class
import           Data.BoundingBox
import qualified Data.List              as L
import qualified Data.BoundingBox.Range as R

type BBoxOffset = Scalar
data Branch = BLeft | BRight deriving Show
type LeftRange  = R.Range
type RightRange = R.Range

-- | KdTree adapted to regions, based on
--   Foundations of Multidimensional and Metric Data Structures by Hanan Samet

class ( BoundingBox bbox) => KdTreeRegional bbox where
  data Axes bbox :: *
  data KdTree bbox :: * -> *
  data Leaf bbox :: * -> *
  data Collisions bbox :: * -> *
  type Region bbox :: *
  type Vect bbox :: *
  type Nearest bbox a :: *

  -- | toBox builds a list of (BBox3,(Midpoint,payload))
  --   given (vector, offset, payload)
  toBBox :: (Vect bbox,BBoxOffset,a) -> (Vect bbox,(bbox,a))
  -- | toList generates list of bbox/payload pairs, given a KdTree
  toList :: KdTree bbox a -> [(bbox,a)]
  -- | fromList builds KdTree given list of bboxes/payload pairs
  fromList :: [(bbox,a)] -> KdTree bbox a

  -- | fromSubList
  fromSubList :: bbox -> [(bbox,a)] -> Axes bbox -> KdTree bbox a

  -- | insert t (b,a) inserts (b,a) in t
  insert :: KdTree bbox a -> (bbox,a) -> KdTree bbox a

  -- | remove t (b,a) removes (b,a) from t
  remove :: KdTree bbox a -> (bbox,a) -> KdTree bbox a

  -- | nearestNeighbor returns the nearest neighbor of bbox in tree.
  nearestNeighbor :: KdTree bbox a                        ->
                     bbox                                 ->
                     Either (Collisions bbox a) (Maybe [(Scalar,bbox,a)])

  -- | nearNeighbors kdtree r bbox returns all neighbors within 
  --   distance r from bbox in tree.
  nearNeighbors :: KdTree bbox a -> Scalar -> bbox -> Maybe [(Scalar,bbox,a)]

  -- | kNearestNeighbors tree k bbox returns the k closest points
  --   to bbox within tree.
  kNearestNeighbors :: KdTree bbox a -> Int -> bbox -> [(Scalar,bbox,a)]

  findNearest :: KdTree bbox a -> bbox -> [(Scalar,bbox,a)]

