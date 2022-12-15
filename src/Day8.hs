{-# LANGUAGE ScopedTypeVariables #-}
module Day8 (day8Part1, day8Part2) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, filterWithKey, keys, delete, alter, singleton, insert, lookup)
import Data.Map as Map (foldr, map, size, filter)

type TreeSize = Int
type NbTree = Int
type Col = Int
type Row = Int
type VisibleTreesAcc = Int

day8Part1 :: [String] -> IO ()
day8Part1 lines =
  let trees :: [[Int]] = Prelude.map (Prelude.map (read . (:[]))) lines
      visibleTrees = scanTrees trees 0 0 0 empty empty empty 0
  in putStrLn (show visibleTrees <> " visible trees")

scanTrees :: [[Int]] -> Row -> Col -> TreeSize -> Map Col TreeSize -> Map (Int, Int) TreeSize -> Map Col (Map (Int, Int) TreeSize) -> Int -> Int
scanTrees [] currentRow currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleTreesOnCols = Map.size potentiallyVisibleTreesFromBottom
   in visibleTreesOnCols + acc
scanTrees ([]:rest) currentRow currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleTreesFromRight = Map.size potentiallyVisibleTreesFromRight
      newPotentiallyVisibleFromBottom = maybe empty (\colMap -> insert currentCol (go colMap (keys potentiallyVisibleTreesFromRight)) potentiallyVisibleTreesFromBottom) (lookup currentCol potentiallyVisibleTreesFromBottom)
        where
          go m [] = m
          go m (k:rest) = go (delete k m) rest
   in scanTrees rest (currentRow + 1) 0 0 biggestTreeOnCol empty newPotentiallyVisibleFromBottom acc + visibleTreesFromRight
scanTrees ((tree:otherTrees):rest) currentRow currentCol biggestTreeOnRow biggestTreeOnCol potentiallyVisibleTreesFromRight potentiallyVisibleTreesFromBottom acc =
  let visibleFromLeft = tree > biggestTreeOnRow
      newBiggestTreeOnRow = if visibleFromLeft then tree else biggestTreeOnRow

      visibleFromTop = maybe True (tree >) $ lookup currentCol biggestTreeOnCol
      newBiggestTreeOnCol = if visibleFromTop then insert currentCol tree biggestTreeOnCol else biggestTreeOnCol

      visible = visibleFromLeft || visibleFromTop
      newAcc = if visible then acc + 1 else acc

      potentiallyVisibleFromRightUpdated = Map.filter (> tree) potentiallyVisibleTreesFromRight
      newPotentiallyVisibleFromRight = 
        if visible then potentiallyVisibleFromRightUpdated
          else insert (currentRow, currentCol) tree potentiallyVisibleFromRightUpdated 

      potentiallyVisibleFromBottomUpdated = maybe potentiallyVisibleTreesFromBottom (\colMap -> insert currentCol (Map.filter (> tree) colMap) potentiallyVisibleTreesFromBottom) (lookup currentCol potentiallyVisibleTreesFromBottom)
      newPotentiallyVisibleFromBottom = 
        if visible then potentiallyVisibleFromBottomUpdated
          else alter (maybe (Just $ singleton (currentRow, currentCol) tree) (\colMap -> Just $ insert (currentRow, currentCol) tree potentiallyVisibleFromRightUpdated)) currentCol potentiallyVisibleFromBottomUpdated
  in scanTrees (otherTrees:rest) currentRow (currentCol + 1) newBiggestTreeOnRow newBiggestTreeOnCol newPotentiallyVisibleFromRight newPotentiallyVisibleFromBottom newAcc

day8Part2 :: [String] -> IO ()
day8Part2 (firstLine:rest) = putStrLn "Not yet implemented"
