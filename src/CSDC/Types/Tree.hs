module CSDC.Types.Tree
  ( treeMap
  ) where

import CSDC.Declaration

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

treeMap ::
  Ord a =>
  (String -> a) -> (String -> [String] -> b) ->
  Tree -> Map a b
treeMap f g = Map.fromList . treeAssocs f g

treeAssocs ::
  (String -> a) -> (String -> [String] -> b) ->
  Tree -> [(a, b)]
treeAssocs f g (Branch n b) =
  (f n, g n (fmap rootLabel b)) : concatMap (treeAssocs f g) b
treeAssocs _ _ (Leaf _) = []

rootLabel :: Tree -> String
rootLabel (Branch n _) = n
rootLabel (Leaf n) = n
