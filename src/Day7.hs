{-# LANGUAGE ScopedTypeVariables #-}
module Day7 (day7Part1, day7Part2) where

import Prelude hiding (lookup)
import Prelude (map)
import Data.Map (Map, empty, insert, lookup, keys, elems, (!))
import Data.Map as Map (map, filter)
import Data.Maybe (maybe, fromJust)
import Data.Semigroup (Min(Min))
import Debug.Trace (trace)

day7Part1 :: [String] -> IO ()
day7Part1 (firstLine:rest) = do
  myMap <- dirMap ["/"] empty rest
  total <- computeTotal myMap
  putStrLn ("Total: " <> show total)

dirMap :: [String] -> Map [String] (Int, [[String]]) -> [String] -> IO (Map [String] (Int, [[String]]))
dirMap currentDir m [] = return m
dirMap currentDir m (l:rest) = go currentDir m (l:rest)

go :: [String] -> Map [String] (Int, [[String]]) -> [String] -> IO (Map [String] (Int, [[String]]))
go currentDir m ("$ ls":rest) = putStrLn "LS command" >> dirMap currentDir m rest
go currentDir m (('d':'i':'r':' ':dirName):rest) = putStrLn "Dir line" >> dirMap currentDir (maybe (insert currentDir (0, [currentDir ++ [dirName]]) m) (\(oldSum, subDirs) -> insert currentDir (oldSum, (currentDir ++ [dirName]):subDirs) m) (lookup currentDir m)) rest
go currentDir m ("$ cd ..":rest) = putStrLn "CD .." >> dirMap (init currentDir) m rest
go currentDir m (('$':' ':'c':'d':' ':dirName):rest) = putStrLn ("cd to dir " <> dirName) >> dirMap (currentDir ++ [dirName]) m rest
go currentDir m (line:rest) = dirMap currentDir newMap rest
  where
    (sizeStr:fileName:[]) = words line
    size :: Int = read sizeStr
    newMap = maybe (insert currentDir (size, []) m) (\(oldSum, subDirs) -> insert currentDir (oldSum + size, subDirs) m) (lookup currentDir m)

computeTotal :: Map [String] (Int, [[String]]) -> IO Int
computeTotal m =
   return $ foldl (+) 0 $ Prelude.filter (<= 100000) (elems (mapOfSizes m))

mapOfSizes :: Map [String] (Int, [[String]]) -> Map [String] Int
mapOfSizes m = Map.map (\dirSpec -> computeDirSize dirSpec m) m

computeDirSize :: (Int, [[String]]) -> Map [String] (Int, [[String]]) -> Int
computeDirSize (filesSize, subDirs) m =
  let subDirSize = foldl (+) 0 $ Prelude.map (\d -> computeDirSize (fromJust (lookup d m)) m) subDirs
  in filesSize + subDirSize

isSubDir :: [String] -> [String] -> Bool
isSubDir [] _ = True
isSubDir _ [] = False
isSubDir (d1:rest1) (d2:rest2)
  | d1 /= d2 = False
  | otherwise = isSubDir rest1 rest2

dirSize :: [String] -> Map [String] [Int] -> Int
dirSize dir m = foldl (+) 0 (m ! dir)

day7Part2 :: [String] -> IO ()
day7Part2 (firstLine:rest) = do
  myMap <- dirMap ["/"] empty rest
  let sizes = mapOfSizes myMap
      totalSize = fromJust (lookup ["/"] sizes)
      freeSpace = 70000000 - totalSize
      spaceToFree = 30000000 - freeSpace
      possibleDirs = Map.filter (\s -> s >= spaceToFree) sizes
  putStrLn ("Size Of the dir to delete:" <> (show $ foldMap Min possibleDirs))
