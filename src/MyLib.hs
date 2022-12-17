module MyLib (someFunc) where

import System.Environment (getArgs)
import Data.Map.Strict (Map, fromList, singleton, empty, (!?))
import Day1 (day1Part1, day1Part2)
import Day2 (day2Part1, day2Part2)
import Day3 (day3Part1, day3Part2)
import Day4 (day4Part1, day4Part2)
import Day5 (day5Part1, day5Part2)
import Day6 (day6Part1, day6Part2)
import Day7 (day7Part1, day7Part2)
import Day8 (day8Part1, day8Part2)
import Day9 (day9Part1, day9Part2)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let argsMap = parseArgs args
      day  = argsMap !? "day"
      part  = argsMap !? "part"
      solvingFunc = ((,) <$> day <*> part) >>= (\k -> solversMap !? k >>= (\f -> return (f, k)))
  case solvingFunc of
    Nothing -> putStrLn ("Error, unable to parse args:" <> show argsMap)
    Just (f, (d, p)) -> do
      putStrLn ("solving problem of day " <> show d <> ", part " <> show p)
      fileContent <- readFile ("resources/day" <> show d)
      f (lines fileContent)

solversMap :: Map (Int, Int) ([String] -> IO())
solversMap = fromList [((1, 1), day1Part1), ((1, 2), day1Part2), ((2, 1), day2Part1), ((2, 2), day2Part2), ((3, 1), day3Part1), ((3, 2), day3Part2), ((4, 1), day4Part1), ((4, 2), day4Part2), ((5, 1), day5Part1), ((5, 2), day5Part2), ((7, 1), day6Part1), ((6, 2), day6Part2), ((7, 1), day7Part1), ((7, 2), day7Part2), ((8, 1), day8Part1), ((8, 2), day8Part2), ((9, 1), day9Part1), ((9, 2), day9Part2)]

parseArgs :: [String] -> Map String Int
parseArgs args = go args empty
  where
    go ("--day":day:rest) map = go rest $ map <> (singleton "day" (read day :: Int))
    go ("--part":part:rest) map = go rest $ map <> (singleton "part" (read part))
    go _ map = map
