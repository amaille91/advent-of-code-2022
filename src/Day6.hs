module Day6 (day6Part1, day6Part2) where

import Data.Set (fromList, size)

day6Part1 :: [String] -> IO ()
day6Part1 (firstLine:[]) = go 4 (reverse (take 4 firstLine)) (drop 4 firstLine)
  where
    go n last4 rest
      | size (fromList last4) == 4 = putStrLn ("Total: " <> show n)
      | otherwise = go (n+1) (head rest:take 3 last4) (tail rest)

day6Part2 :: [String] -> IO ()
day6Part2 (firstLine:[]) = go 14 (reverse (take 14 firstLine)) (drop 14 firstLine)
  where
    go n last4 rest
      | size (fromList last4) == 14 = putStrLn ("Total: " <> show n)
      | otherwise = go (n+1) (head rest:take 13 last4) (tail rest)
