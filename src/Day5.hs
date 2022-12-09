module Day5 (day5Part1, day5Part2) where

import Control.Applicative ((<|>))
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Identity (Identity)
import Data.List (sort, foldr)
import Data.Semigroup (Max(Max))
import Data.String (fromString)
import Data.Monoid (Sum(Sum))
import Data.Map (Map, empty, unionsWith, insert, (!), keys)
import Data.Char (toLower)
import Debug.Trace (trace)
import Text.Parsec (Parsec, Stream, runParser, getState, modifyState, many, space, optional, letter, char)
import Text.Parsec.Error (messageString, errorMessages)

data Move = Move Int Int Int

day5Part1 :: [String] -> IO ()
day5Part1 lines =
  let (startingState, moves) = parseInput lines
  in go startingState moves
  where
    go state [] = putStrLn ("Top crates: " <> concatHeads state)
    go state (Move nb from to:rest) =
      let newMap = goBis nb from to state
      in go newMap rest
    goBis 0 _ _ state = state
    goBis nb from to state =
      let (crate:stack) = state ! from
          tempState = insert from stack state
      in goBis (nb - 1) from to (insert to (crate:tempState ! to) tempState)

parseInput :: [String] -> (Map Int [Char], [Move])
parseInput lines =
  let (stateLines, _empty:moveLines) = break (== "") lines
  in (parseState $ init stateLines, parseMoves moveLines)

parseMoves :: [String] -> [Move]
parseMoves lines = map parseMove lines
  where parseMove l =
          let wd = words l
          in Move (read $ wd !! 1) (read $ wd !! 3) (read $ wd !! 5)

parseState :: [String] -> Map Int [Char]
parseState lines =
  let crateLines = map parseStateLine lines
   in unionsWith (++) crateLines

parseStateLine :: String -> Map Int [Char]
parseStateLine line =
  case runParser stateParser (1, empty) "input crate" (fromString line) of
    Right m -> m
    Left err -> error "Parsing error"
  where
    stateParser :: Parsec ByteString (Int, Map Int [Char]) (Map Int [Char])
    stateParser = do
      crateParser
      (_, finalMap) <- getState
      return finalMap

crateParser :: Parsec ByteString (Int, Map Int [Char]) [()]
crateParser = many $ ((parseEmptyCrate <|> parseFullCrate) >>= (\crateString -> modifyState (\(currentCrate, m) -> (currentCrate + 1, insert currentCrate crateString m))))
  where
    parseEmptyCrate = space >> space >> space >> optional space >> return ""
    parseFullCrate = char '[' >> letter >>= (\c -> char ']' >> optional space >> return (c:[]))

concatHeads :: Map Int [Char] -> String
concatHeads m = foldr (:) [] $ map (head . (m !)) $ sort (keys m)

day5Part2 :: [String] -> IO ()
day5Part2 lines =
  let (startingState, moves) = parseInput lines
  in go startingState moves
  where
    go state [] = putStrLn ("Top crates: " <> concatHeads state)
    go state (Move nb from to:rest) =
      let fromStack = state ! from
          newFromstack = drop nb fromStack
          toStack = state ! to
          newTostack = take nb fromStack <> toStack
          newMap = insert from newFromstack (insert to newTostack state)
      in go newMap rest
