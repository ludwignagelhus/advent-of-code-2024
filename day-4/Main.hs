module Main where

import Text.Read (readMaybe)
import TestModule

main :: IO ()
main = do
    putStrLn "day 4"
    puzzle <- readInput
    return ()


readInput :: IO Puzzle
readInput = do
    lines <- lines <$> readFile "input-example.txt"
    return lines

type Puzzle = [String]
type PuzzleChar = (Coord, Char)

-- data PuzzleChar = PuzzleChar Coord Char deriving (Show, Eq)

-- -- maybe only need to check right, down, down-right, down-left
-- getCharSeqs :: Coord -> [String] -> [[Char]]
-- getCharSeqs orig puzzle =
--     -- let orig = Just (puzzle !! y !! x)
--     let east = map puzzleChar . addCoords orig [(1,0), (2,0), (3,0)]
--     let southEast = map puzzleChar [orig, (), (), ()] lines !! (y + 1) !! (x + 1)
--     let south = []
--     let southWest = []

findXmas :: Coord -> Puzzle -> [PuzzleChar]
findXmas orig puzzle =

puzzleChar :: Puzzle -> Coord -> Maybe PuzzleChar
puzzleChar puzzle (x,y) =
    if length puzzle <= y|| length (puzzle !! y) <= x then Nothing
    else Just ((x,y), puzzle !! y !! x)

isXmas :: [Char] -> Bool
isXmas word = word == "xmas" || reverse word == "xmas"
   
        