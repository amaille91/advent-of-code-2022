module Day1 (day1Part1, day1Part2) where

import Data.Semigroup (Max(Max))
import Data.Monoid (Sum(Sum))
import Data.List (sort)

day1Part1 :: [String] -> IO ()
day1Part1 lines = go 0 lines
  where
    go maxAcc [] = putStrLn $ "Max elf stock: " <> show maxAcc
    go maxAcc ("":rest) = go maxAcc rest
    go maxAcc lines =
      go max rest
        where
          (elf, rest) = span (/= "") lines
          Sum elfStock = foldMap Sum (map read elf)
          Max max = Max maxAcc <> Max elfStock

day1Part2 :: [String] -> IO ()
day1Part2 lines =
  let sortedCalories = reverse $ sort elvesCalories
      Sum bestThree = foldMap Sum (take 3 sortedCalories)
      elvesCalories :: [Int]
      elvesCalories = go [] lines
        where go acc [] = acc
              go acc ("":rest) = go acc rest
              go acc l =
                let (elf, rest) = span (/= "") l
                    Sum elfCal = foldMap Sum (map read elf)
                in go (elfCal:acc) rest
  in putStrLn ("Top three elves carry " <> show bestThree <> " calories")
