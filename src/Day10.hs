module Day10 (day10Part1, day10Part2) where


day10Part1 :: [String] -> IO ()
day10Part1 lines = do
  signalsSum <- handleInstructions lines 1 1 0
  putStrLn ("The sum of signals is " <> show signalsSum)

handleInstructions :: [String] -> Int -> Int -> Int -> IO Int
handleInstructions [] _ _ acc = return acc
handleInstructions ("noop":rest) cycle register acc = do
  newAcc <- computeNewAcc cycle register acc
  handleInstructions rest (cycle + 1) register newAcc
handleInstructions (('a':'d':'d':'x':' ':addReg):rest) cycle register acc = do
  newAcc <- computeNewAcc cycle register acc
  newAcc2 <- computeNewAcc (cycle + 1) register newAcc
  handleInstructions rest (cycle + 2) (register + (read addReg)) newAcc2

computeNewAcc cycle register acc
  | cycle == 20 || cycle `rem` 40 == 20 && cycle <= 220 = 
    let power = (cycle * register)
    in do
      putStrLn ("cycle " <> show cycle <> " with register " <> show register <> " gives " <> show power <> " power") 
      return $ acc + power
  | otherwise = return acc

day10Part2 :: [String] -> IO ()
day10Part2 lines = do
  crt <- handleInstructions2 lines 1 1 [] []
  sequence_ (map putStrLn crt)

handleInstructions2 :: [String] -> Int -> Int -> String -> [String] -> IO [String]
handleInstructions2 [] _ _ _ reversedCrt = return $ reverse reversedCrt
handleInstructions2 ("noop":rest) cycle register reversedCrtLine reversedCrt =
  let (newReversedLine, newReversedCrt) = doCycle cycle register reversedCrtLine reversedCrt
  in handleInstructions2 rest (cycle + 1) register newReversedLine newReversedCrt
handleInstructions2 (('a':'d':'d':'x':' ':addReg):rest) cycle register reversedCrtLine reversedCrt =
  let (newReversedLine, newReversedCrt) = doCycle cycle register reversedCrtLine reversedCrt
      (newReversedLine2, newReversedCrt2) = doCycle (cycle + 1) register newReversedLine newReversedCrt
  in handleInstructions2 rest (cycle + 2) (register + (read addReg)) newReversedLine2 newReversedCrt2

doCycle cycle register reversedCrtLine reversedCrt =
  let horizontalCrtPos = (cycle - 1) `rem` 40
      newLine = (if (horizontalCrtPos <= register + 1 && horizontalCrtPos >= register - 1) then  '#' else '.'):reversedCrtLine
      newReversedCrt = if lastPixel cycle then (reverse newLine):reversedCrt else reversedCrt
  in (if lastPixel cycle then [] else newLine, newReversedCrt)
  where lastPixel c = c `rem` 40 == 0
