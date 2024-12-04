module Main where

import Text.Read (readMaybe)
import Util

main :: IO ()
main = do
    putStrLn "day 4"
    puzzle <- readInput

    print $ findXmas (5,0) puzzle

    print $ "foo" ++ ""

    return ()


readInput :: IO Puzzle
readInput = do
    lines <$> readFile "input-example.txt"

type Puzzle = [String]
type PuzzleChar = (Coord, Char)

-- data PuzzleChar = PuzzleChar Coord Char deriving (Show, Eq)

-- -- maybe only need to check right, down, down-riqght, down-left
-- getCharSeqs :: Coord -> [String] -> [[Char]]
-- getCharSeqs orig puzzle =
--     -- let orig = Just (puzzle !! y !! x)
--     let east = map puzzleChar . addCoords orig [(1,0), (2,0), (3,0)]
--     let southEast = map puzzleChar [orig, (), (), ()] lines !! (y + 1) !! (x + 1)
--     let south = []
--     let southWest = []

    -- erf 

-- findXmas :: Coord -> Puzzle -> [Char]
findXmas orig puzzle =
    let east = map (getPChar puzzle . addCoords orig) [(0,0), (1,0), (2,0), (3,0)]
        southEast = filter Just $ map (getPChar puzzle . addCoords orig) [(0,0), (1,1), (2,2), (3,3)]
        south = filter Just $ map (getPChar puzzle . addCoords orig) [(0,0), (0,1), (0,2), (0,3)]
        southWest = filter Just $ map (getPChar puzzle . addCoords orig) [(0,0), (-1,1), (-2,2), (-3,3)]
    in
        east
        

getPChar :: Puzzle -> Coord -> Maybe Char
getPChar puzzle (x,y) =
    if length puzzle <= y|| length (puzzle !! y) <= x then Nothing
    else Just (puzzle !! y !! x)

puzzleChar :: Puzzle -> Coord -> Maybe PuzzleChar
puzzleChar puzzle (x,y) =
    if length puzzle <= y|| length (puzzle !! y) <= x then Nothing
    else Just ((x,y), puzzle !! y !! x)

isXmas :: [Char] -> Bool
isXmas word = word == "xmas" || reverse word == "xmas"

        