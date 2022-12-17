module Day9 (day9Part1, day9Part2) where

import Data.Set (fromList, size, insert)

day9Part1 :: [String] -> IO ()
day9Part1 lines =
  let parsedLines = lines >>= (\(char:_:nb) -> take (read nb) (repeat char))
  in do
    places <- go parsedLines (0,0) (0,0) (fromList [(0,0)])
    putStrLn ((show $ places) <> " places visited by the tail")
    where
      go [] _ _ places = return $ size places
      go (c:rest) head tail visitedPlaces =
        let newHead = computeNewHead c head
            newTail = computeNewTail newHead tail
        in do
          putStrLn ("New tail: " <> show newTail)
          go rest newHead newTail (insert newTail visitedPlaces)

computeNewHead :: Char -> (Int, Int) -> (Int, Int)
computeNewHead 'L' (row, col) = (row - 1, col)
computeNewHead 'U' (row, col) = (row, col + 1)
computeNewHead 'D' (row, col) = (row, col - 1)
computeNewHead 'R' (row, col) = (row + 1, col)

computeNewTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
computeNewTail (headRow, headCol) (tailRow, tailCol)
  | headRow == tailRow = if headCol - tailCol == 2 then (tailRow, tailCol + 1) else if headCol - tailCol == -2 then (tailRow, tailCol - 1) else (tailRow, tailCol)
  | headCol == tailCol = if headRow - tailRow == 2 then (tailRow + 1, tailCol) else if headRow - tailRow == -2 then (tailRow - 1, tailCol) else (tailRow, tailCol)
  | headCol > tailCol + 1 = if headRow < tailRow then (tailRow - 1, tailCol + 1) else (tailRow + 1, tailCol + 1)
  | headCol < tailCol - 1 = if headRow < tailRow then (tailRow - 1, tailCol - 1) else (tailRow + 1, tailCol - 1)
  | headRow > tailRow + 1 = if headCol < tailCol then (tailRow + 1, tailCol - 1) else (tailRow + 1, tailCol + 1)
  | headRow < tailRow - 1 = if headCol < tailCol then (tailRow - 1, tailCol - 1) else (tailRow - 1, tailCol + 1)
  | otherwise = (tailRow, tailCol)

day9Part2 :: [String] -> IO ()
day9Part2 lines =
  let parsedLines = lines >>= (\(char:_:nb) -> take (read nb) (repeat char))
  in do
    places <- go parsedLines (0,0) (take 9 $ repeat (0,0)) (fromList [(0,0)])
    putStrLn ((show $ places) <> " places visited by the tail")
    where
      go [] _ _ places = return $ size places
      go (c:rest) head tails visitedPlaces =
        let newHead = computeNewHead c head
            (lastTail, newTails) = foldl (\(h, newTailsReversed) t -> let newTail = computeNewTail h t in (newTail, newTail:newTailsReversed)) (newHead, []) tails
        in do
          go rest newHead (reverse newTails) (insert lastTail visitedPlaces)
