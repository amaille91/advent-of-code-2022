module Day3 (day3Part1, day3Part2) where

import Data.Semigroup (Max(Max))
import Data.Monoid (Sum(Sum))
import Data.Set (Set, fromList, toList, empty, intersection)
import Data.Char (toLower)
import Debug.Trace (trace)

type AdversaryPlay = String
type MyPlay = String

day3Part1 :: [String] -> IO ()
day3Part1 lines =
  let misplacedTypesOfItems = concatMap (toList . computeMisplacedItems) lines
      prioritiesOfMisplacedItems = map priority misplacedTypesOfItems
      Sum total = foldMap Sum prioritiesOfMisplacedItems
  in putStrLn ("Total: " <> show total)

computeMisplacedItems :: String -> Set Char
computeMisplacedItems s =
  let (first, second) = splitAt (length s `div` 2) s
  in intersection (fromList first) (fromList second)

priority :: Char -> Int
priority 'a' = 1
priority 'b' = 2
priority 'c' = 3
priority 'd' = 4
priority 'e' = 5
priority 'f' = 6
priority 'g' = 7
priority 'h' = 8
priority 'i' = 9
priority 'j' = 10
priority 'k' = 11
priority 'l' = 12
priority 'm' = 13
priority 'n' = 14
priority 'o' = 15
priority 'p' = 16
priority 'q' = 17
priority 'r' = 18
priority 's' = 19
priority 't' = 20
priority 'u' = 21
priority 'v' = 22
priority 'w' = 23
priority 'x' = 24
priority 'y' = 25
priority 'z' = 26
priority c = priority (toLower c) + 26

day3Part2 :: [String] -> IO ()
day3Part2 lines =
  let groups = go [] lines
      go acc [] = acc
      go acc l = go (take 3 l:acc) (drop 3 l)
      badges = map computeBadge groups
      Sum total = foldMap Sum (map priority badges)
  in putStrLn $ "Total: " <> show total

computeBadge :: [String] -> Char
computeBadge ss = head $ toList $ foldl intersection (fromList $ head ss) $ map fromList ss
