module Main where

import Data.List (permutations)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Util

main :: IO ()
main = do
    putStrLn "day 4"
    puzzle <- readInput

    let nRows = length puzzle
    let nCols = length (head puzzle)
    let coords = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]

    let total = foldl (\acc coord -> acc + countXmas puzzle coord) 0 coords

    print total

readInput :: IO Puzzle
readInput = 
    lines <$> readFile "input-example.txt"

type Puzzle = [String]
type PuzzleChar = (Coord, Char)

countXmas :: Puzzle -> Coord -> Int
countXmas puzzle orig =
    let east = mapMaybe (getPChar puzzle . addCoords orig) [(0,0), (1,0), (2,0), (3,0)]
        southEast = mapMaybe (getPChar puzzle . addCoords orig) [(0,0), (1,1), (2,2), (3,3)]
        south = mapMaybe (getPChar puzzle . addCoords orig) [(0,0), (0,1), (0,2), (0,3)]
        southWest = mapMaybe (getPChar puzzle . addCoords orig) [(0,0), (-1,1), (-2,2), (-3,3)]
    in
       length $ filter isXmas [east, southEast, south, southWest]
        
-- would be cool to test some maybe stuff here...

getPChar :: Puzzle -> Coord -> Maybe Char
getPChar puzzle (x,y) =
    if x < 0 || y < 0 || length puzzle <= y 
        || length (puzzle !! y) <= x then Nothing
    else Just (puzzle !! y !! x)

puzzleChar :: Puzzle -> Coord -> Maybe PuzzleChar
puzzleChar puzzle (x,y) =
    if length puzzle <= y|| length (puzzle !! y) <= x then Nothing
    else Just ((x,y), puzzle !! y !! x)

isXmas :: [Char] -> Bool
isXmas word = word == "XMAS" || reverse word == "XMAS"

        