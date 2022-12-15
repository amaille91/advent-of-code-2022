{-# LANGUAGE ScopedTypeVariables #-}
module Day8 (day8Part1, day8Part2) where

import Data.Map (Map, empty, filterWithKey)
import Data.Map as Map (foldr, map)

type TreeSize = Int
type NbTree = Int
type Col = Int
type VisibleTreesAcc = Int

day8Part1 :: [String] -> IO ()
day8Part1 lines =
  let trees :: [[Int]] = Prelude.map (Prelude.map (read . show)) lines
      visibleTrees = scanTrees trees 0 0 empty empty empty 0
  in putStrLn (show visibleTrees <> " visible trees")

scanTrees :: [[Int]] -> Col -> TreeSize -> Map Col TreeSize -> Map TreeSize NbTree -> Map Col (Map TreeSize Int) -> Int -> Int
scanTrees [] currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleTreesOnCols = Map.foldr (+) 0 (Map.map (Map.foldr (+) 0) potentiallyVisibleTreesFromBottom)
   in visibleTreesOnCols + acc
scanTrees ([]:rest) currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleTreesFromRight = Map.foldr (+) 0 potentiallyVisibleTreesFromRight
   in scanTrees rest 0 biggestTreeOnCol empty potentiallyVisibleTreesFromBottom (acc + visibleTreesFromRight)
scanTrees ((tree:otherTrees):rest) currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleFromLeft = tree > biggestTreeOnRow
      newBiggestTreeOnRow = if visibleFromLeft then tree else biggestTreeOnRow

      visibleFromTop = maybe True (lookup currentCol biggestTreeOnCol >>= (\bigTree -> tree > bigTree))
      newBiggestTreeOnCol = if visibleFromTop then insert currentCol tree biggestTreeOnCol else biggestTreeOnCol

      potentiallyVisibleFromRightUpdated = Map.filterWithKey (> tree) potentiallyVisibleTreesFromRight
      newpotentiallyVisibleFromRight = maybe (insert tree 1 potentiallyVisibleFromRightUpdated) (lookup currentCol potentiallyVisibleFromRightUpdated >>= (\oldVal -> insert tree (oldVal + 1)))

day8Part2 :: [String] -> IO ()
day8Part2 (firstLine:rest) = putStrLn "Not yet implemented"
