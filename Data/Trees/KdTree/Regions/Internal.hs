{- |
   Module : Data.Trees.KdTree.Regions.Internal
   Copyright  : Copyright (c) 2016, Michael Litchard
   License    : BSD3
                       
   Maintainer : Michael Litchard
   Stability  : experimental
   Portability: not portable

   This module provides internal functions for all KdTreeRegional
   instances.
-}

module Data.Trees.KdTree.Regions.Internal 
  ( boxAxisDistance
  , splitRange
  , findDistances
  , evalBox
  , filterCandidates
  , distance ) where

import           Data.Vector.Class
import           Data.BoundingBox
import qualified Data.BoundingBox.Range as R

type BBoxOffset = Scalar
data Branch = BLeft | BRight deriving Show
type LeftRange  = R.Range
type RightRange = R.Range

-- property test distance(a, b) + distance(b, c) >= distance(a, c)
-- distance is non-negative
boxAxisDistance :: (BoundingBox a) => a -> a -> (a -> R.Range) -> Scalar
boxAxisDistance bbox1 bbox2 findRange
  | min1 > max2 = (min1 - max2)
  | min2 > max1 = (min2 - max1)
  | otherwise     = 0 -- collision
    where
      min1 = R.min_point range1
      max1 = R.max_point range1
      min2 = R.min_point range2
      max2 = R.max_point range2
      range1 = findRange bbox1
      range2 = findRange bbox2

splitRange :: (BoundingBox a) =>
              (a -> R.Range)  ->
              Scalar          ->
              a               ->
              (LeftRange,RightRange)
splitRange = error ("splitRange incomplete")

findDistances :: (BoundingBox b)    =>
                 (Scalar, b, a)     ->
                 Either (b,a) (Scalar,b,a)
findDistances = error ("findDistances incomplete")

evalBox :: (BoundingBox a) => (a -> R.Range) -> a -> Scalar -> Bool
evalBox = error ("evalBox incomplete")

-- | filterCandidates
filterCandidates :: (BoundingBox b) => [(Scalar,b,a)] -> [(Scalar,b,a)]
filterCandidates = error ("filterCandidates incomplete")

distance (d1,_,_) (d2,_,_) = error ("distance incomplete")
