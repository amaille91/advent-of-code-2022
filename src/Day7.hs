module Day7 (day7Part1, day7Part2) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, insert, lookup, keys, (!))
import Data.Maybe (maybe)
import Debug.Trace (trace)

day7Part1 :: [String] -> IO ()
day7Part1 (firstLine:rest) =
  let
    myMap = dirMap ["/"] empty rest
    total = trace "computing total" $ computeTotal myMap
  in putStrLn ("Total: " <> show total)
  where
    dirMap currentDir m [] = trace "Over" m
    dirMap currentDir m (l:rest) = go currentDir m (l:rest)
    go currentDir m ("$ ls":rest) = dirMap currentDir m rest
    go currentDir m (('d':'i':'r':_):rest) = dirMap currentDir m rest
    go currentDir m ("$ cd ..":rest) = dirMap (init currentDir) m rest
    go currentDir m (('$':' ':'c':'d':' ':dirName):rest) = dirMap (currentDir ++ [dirName]) m rest
    go currentDir m (line:rest) = trace ("handling file " <> line) $
      let (sizeStr:fileName:[]) = words line
          newMap = maybe (insert currentDir (read sizeStr) m) (\oldVal -> insert currentDir ((read sizeStr):oldVal) m) (lookup currentDir m)
       in dirMap currentDir newMap rest

computeTotal :: Map [String] [Integer] -> Integer
computeTotal m = trace "computing total" $
  let mapOfSizes = map (\dir -> computeDirSize dir m) (keys m)
   in foldl (+) 0 $ filter (<= 100000) mapOfSizes

computeDirSize :: [String] -> Map [String] [Integer] -> Integer
computeDirSize dir m = foldl (+) 0 $ map (\subdir -> if isSubDir dir subdir then dirSize subdir m else 0) (keys m)

isSubDir :: [String] -> [String] -> Bool
isSubDir [] _ = True
isSubDir _ [] = False
isSubDir (d1:rest1) (d2:rest2)
  | d1 /= d2 = False
  | otherwise = isSubDir rest1 rest2

dirSize :: [String] -> Map [String] [Integer] -> Integer
dirSize dir m = foldl (+) 0 (m ! dir)

day7Part2 :: [String] -> IO ()
day7Part2 (firstLine:[]) = putStrLn "Not yet implemented"
