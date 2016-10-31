{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module SplitBox (minMaxBoxSplit) where

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


