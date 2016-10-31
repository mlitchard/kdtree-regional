{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module BoxAxisDistance (nonNegative,symmetry,triangularity) where

import Test.QuickCheck.Arbitrary hiding ((><))
import Test.SmallCheck.Series
import GHC.Generics

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
-- | tests for boxAxisDistance
-- http://www.citr.auckland.ac.nz/~rklette/Books/MK2004/pdf-LectureNotes/07slides.pdf
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
  and $ zipWith (curry symmetricTest) dist1 dist2
  where
    (b1,_) = snd (toBBox (boxInput p))
    (b2,_) = snd (toBBox (boxInput q))
    dist1 = map (boxAxisDistance b1 b2) [rangeX,rangeY,rangeZ]
    dist2 = map (boxAxisDistance b2 b1) [rangeX,rangeY,rangeZ]

triangularity :: ((Vect BBox3,Int),(Vect BBox3,Int), (Vect BBox3,Int)) -> Bool
triangularity (p,q,r) =
  and (map triangularityTest (zip3 dist_pr dist_pq dist_qr))
  where
     (bp,_) = snd (toBBox (boxInput p))
     (bq,_) = snd (toBBox (boxInput q))
     (br,_) = snd (toBBox (boxInput r))
     dist_pr = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_pq = map (boxAxisDistance bp br) [rangeX,rangeY,rangeZ]
     dist_qr = map (boxAxisDistance bq br) [rangeX,rangeY,rangeZ]

-----------
symmetricTest :: (Scalar,Scalar) -> Bool
symmetricTest (d1,d2) = d1 == d2

triangularityTest :: (Scalar,Scalar,Scalar) -> Bool
triangularityTest (pr,pq,qr) =
  pr <= pq + qr
