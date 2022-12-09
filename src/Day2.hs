module Day2 (day2Part1, day2Part2) where

import Data.Semigroup (Max(Max))
import Data.Monoid (Sum(Sum))
import Data.List (sort)
import Debug.Trace (trace)

type AdversaryPlay = String
type MyPlay = String

day2Part1 :: [String] -> IO ()
day2Part1 lines =
  let
    scores = map (uncurry computeScore . getInputs) lines
    Sum total = foldMap Sum scores
  in do
    putStrLn $ "Total score: " <> show total

isSame :: AdversaryPlay -> MyPlay -> Bool
isSame "A" "X" = True
isSame "B" "Y" = True
isSame "C" "Z" = True
isSame _ _ = False

getInputs :: String -> (String, String)
getInputs [adversePlay, _space, myPlay] = (adversePlay:[], myPlay:[])

computePlays :: String -> String -> (String, String)
computePlays adverse "Y" = (adverse, case adverse of
                                       "A" -> "X"
                                       "B" -> "Y"
                                       _ -> "Z")
computePlays adverse "X" = (adverse, case adverse of
                             "A" -> "Z"
                             "B" -> "X"
                             _ -> "Y")
computePlays adverse _ = (adverse, case adverse of
                             "A" -> "Y"
                             "B" -> "Z"
                             _ -> "X")

computeScore :: AdversaryPlay -> MyPlay -> Int
computeScore adverse my = computeVictoryScore adverse my + computeMyPlayScore my

computeVictoryScore :: AdversaryPlay -> MyPlay -> Int
computeVictoryScore adverse my
  | isSame adverse my = 3
  | otherwise = case (adverse, my) of
                  ("A", "Z") -> 0
                  ("B", "X") -> 0
                  ("C", "Y") -> 0
                  _ -> 6

computeMyPlayScore :: MyPlay -> Int
computeMyPlayScore "X" = 1
computeMyPlayScore "Y" = 2
computeMyPlayScore _ = 3

day2Part2 :: [String] -> IO ()
day2Part2 lines =
  let
    scores = map (uncurry computeScore . uncurry computePlays . getInputs) lines
    Sum total = foldMap Sum scores
  in do
    putStrLn $ "Total score: " <> show total
