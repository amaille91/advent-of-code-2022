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

logg :: String -> IO ()
logg s = return ()

day8Part1 :: [String] -> IO ()
day8Part1 lines = do
  let trees :: [[Int]] = Prelude.map (Prelude.map (read . (:[]))) lines
  visibleTrees <- scanTrees trees 0 0 (-1) empty 0
  putStrLn (show visibleTrees <> " visible trees")

scanTrees :: [[Int]] -> Row -> Col -> TreeSize -> Map Col TreeSize -> Int -> IO Int
scanTrees [] currentRow currentCol biggestTreeOnRow biggestTreeOnCol acc =
--  let visibleTreesOnCols = Map.foldr (+) 0 $ Map.map Map.size potentiallyVisibleTreesFromBottom
--      newAcc = acc + visibleTreesOnCols
   do
     logg "Finished grid"
--     logg ("Visible from bottom " <> show potentiallyVisibleTreesFromBottom)
     logg ("Final count " <> show acc)
     return $ acc

scanTrees ([]:rest) currentRow currentCol biggestTreeOnRow biggestTreeOnCol acc = do
     logg ("Finished row " <> show currentRow)
     logg ("New acc " <> show acc)
--     logg ("Potentially from bottom " <> show potentiallyVisibleTreesFromBottom)
     logg ""
     scanTrees rest (currentRow + 1) 0 (-1) biggestTreeOnCol acc

scanTrees ((tree:otherTrees):rest) currentRow currentCol biggestTreeOnRow biggestTreeOnCol acc =
  let visibleFromLeft = tree > biggestTreeOnRow
      newBiggestTreeOnRow = if visibleFromLeft then tree else biggestTreeOnRow

      visibleFromTop = maybe True (tree >) $ lookup currentCol biggestTreeOnCol
      newBiggestTreeOnCol = if visibleFromTop then insert currentCol tree biggestTreeOnCol else biggestTreeOnCol

      visibleFromRight = all (< tree) otherTrees

      visibleFromBottom = all (< tree) col
        where col = Prelude.map (!! currentCol) rest

      visible = visibleFromLeft || visibleFromTop || visibleFromRight || visibleFromBottom
      newAcc = if visible then acc + 1 else acc

--      potentiallyVisibleFromBottomUpdated = maybe potentiallyVisibleTreesFromBottom (\colMap -> insert currentCol (Map.filter (> tree) colMap) potentiallyVisibleTreesFromBottom) (lookup currentCol potentiallyVisibleTreesFromBottom)
--      newPotentiallyVisibleFromBottom = 
--        if visible then potentiallyVisibleFromBottomUpdated
--          else alter (maybe (Just $ singleton (currentRow, currentCol) tree) (\colMap -> Just $ insert (currentRow, currentCol) tree colMap)) currentCol potentiallyVisibleFromBottomUpdated
  in do
    logg ("Handled tree " <> show (currentRow, currentCol) <> " " <> show tree)
    logg ("Biggest in row " <> show newBiggestTreeOnRow)
    logg ("Biggest in cols " <> show newBiggestTreeOnCol)
 --   logg ("Potentially bottom " <> show newPotentiallyVisibleFromBottom)
    logg ("Visible fom left " <> show visibleFromLeft)
    logg ("Visible fom right " <> show visibleFromRight)
    logg ("Visible fom top " <> show visibleFromTop)
    logg ("Visible fom bottom " <> show visibleFromBottom)
    logg ("New acc " <> show newAcc)
    logg ""
    scanTrees (otherTrees:rest) currentRow (currentCol + 1) newBiggestTreeOnRow newBiggestTreeOnCol newAcc

day8Part2 :: [String] -> IO ()
day8Part2 lines = do
  let trees :: [[Int]] = Prelude.map (Prelude.map (read . (:[]))) lines
  maxScenicScore <- computeScenicScore trees 0 0 0 [] empty
  putStrLn (show maxScenicScore <> " is the maximum scenic score")

computeScenicScore :: [[Int]] -> Int -> Int -> Int -> [Int] -> Map Col [Int] -> IO Int
computeScenicScore [] maxScenicScore _ _ _ _ = return maxScenicScore
computeScenicScore ([]:rest) maxScenicScore currentRow _ _ colHistory = computeScenicScore rest maxScenicScore (currentRow + 1) 0 [] colHistory
computeScenicScore ((tree:otherTrees):rest) maxScenicScore currentRow currentCol rowHistory colHistory = do
   logg ("Handling tree " <> show (currentRow, currentCol) <> " " <> show tree)
   logg "viewing right "
   viewingRight <- viewingDistance 0 otherTrees tree
   logg "viewing bottom "
   viewingBottom <- viewingDistance 0 col tree
   logg "viewing left "
   viewingLeft <- viewingDistance 0 rowHistory tree
   logg "viewing top "
   viewingTop <- viewingDistance 0 (maybe [] id (lookup currentCol colHistory)) tree
   logg ""
   let scenicScore = viewingRight * viewingLeft * viewingTop * viewingBottom
   computeScenicScore (otherTrees:rest) (max maxScenicScore scenicScore) currentRow (currentCol + 1) (tree:rowHistory) (alter (maybe (Just $ [tree]) (Just . (tree:))) currentCol colHistory)
     where col = Prelude.map (!! currentCol) rest

viewingDistance acc [] _ = logg (show acc) >> return acc
viewingDistance acc (otherTree:[]) tree = logg (show (acc + 1)) >> (return $ acc + 1)
viewingDistance acc (otherTree:rest) tree
  | otherTree < tree = viewingDistance (acc + 1) rest tree
  | otherwise = logg (show (acc + 1)) >> (return $ acc + 1)
