{-# LANGUAGE ScopedTypeVariables #-}
module Day4 (day4Part1, day4Part2) where

import Data.Semigroup (Max(Max))
import Data.Monoid (Sum(Sum))
import Data.Set (Set, fromList, toList, empty, intersection)
import Data.Char (toLower)
import Debug.Trace (trace)

day4Part1 :: [String] -> IO ()
day4Part1 lines = do
  go 0 lines
  where
    go acc [] = putStrLn ("Total: " <> show acc)
    go acc (l:rest) =
      let (firstRange, _comma:secondRange) = break (== ',') l
          (startFirstRange, _dash:endFirstRange) = break (== '-') firstRange
          (startSecondRange, _dash2:endSecondRange) = break (== '-') secondRange
          [sfr, efr, ssr, esr] :: [Int] = map read [startFirstRange, endFirstRange, startSecondRange, endSecondRange]
          newAcc = if sfr <= ssr && efr >= esr || ssr <= sfr && esr >= efr
                    then acc + 1
                    else acc
      in go newAcc rest

day4Part2 :: [String] -> IO ()
day4Part2 lines = do
  go 0 lines
  where
    go acc [] = putStrLn ("Total: " <> show acc)
    go acc (l:rest) =
      let (firstRange, _comma:secondRange) = break (== ',') l
          (startFirstRange, _dash:endFirstRange) = break (== '-') firstRange
          (startSecondRange, _dash2:endSecondRange) = break (== '-') secondRange
          [sfr, efr, ssr, esr] :: [Int] = map read [startFirstRange, endFirstRange, startSecondRange, endSecondRange]
          newAcc = if sfr <= ssr && ssr <= efr || ssr <= sfr && sfr <= esr
                    then acc + 1
                    else acc
      in go newAcc rest
