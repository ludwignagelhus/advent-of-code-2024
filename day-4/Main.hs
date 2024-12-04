module Main where

import Data.List (permutations)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Util

main :: IO ()
main = do
    putStrLn "day 4"
    -- puzzle <- readInput "input.txt"
    -- puzzle <- readInput "input-example.txt"

    -- let nRows = length puzzle
    -- let nCols = length (head puzzle)
    -- let coords = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]

    -- let total = foldl (\acc coord -> acc + countXmas puzzle coord) 0 coords
    -- print total
    -- print $ pt2 puzzle
    pt2

readInput :: String -> IO Puzzle
readInput fileName = 
    lines <$> readFile fileName

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
        
getPChar :: Puzzle -> Coord -> Maybe Char
getPChar puzzle (x,y) =
    if x < 0 || y < 0 || length puzzle <= y 
        || length (puzzle !! y) <= x then Nothing
    else Just (puzzle !! y !! x)

isXmas :: [Char] -> Bool
isXmas word = word == "XMAS" || reverse word == "XMAS"

-- pt2

pt2 :: IO ()
pt2 = do
    puzzle <- readInput "input.txt"
    -- mapM_ print puzzle
    -- print $ getXCoords puzzle (2,1)
    -- print $ isMASAM puzzle (2,1)

    let nRows = length puzzle
    let nCols = length (head puzzle)
    let pixels = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]
    let total = foldl (\acc p -> if isMASAM puzzle p then acc + 1 else acc) 0 pixels
    print total


isMASAM :: Puzzle -> Coord -> Bool
isMASAM puzzle (x,y)
    | length pChars /= 5            = False
    | [nw, orig, se] |> masOrSam
   && [sw, orig, ne] |> masOrSam    = True
    | otherwise                     = False
    where
        coords = getXCoords puzzle (x,y)
        pChars = mapMaybe (getPChar puzzle) coords
        [nw, ne, orig, sw, se] = pChars

getXCoords :: Puzzle -> Coord -> [Coord]
getXCoords puzzle orig =
    [addCoords orig (-1,-1), addCoords orig (1,-1),
     orig,
     addCoords orig (-1,1), addCoords orig (1,1)]

validPCoord :: Puzzle -> Coord -> Bool
validPCoord puzzle (x,y) = x >= 0 && y >= 0 && length puzzle > y && length (puzzle !! y) > x

masOrSam :: [Char] -> Bool
masOrSam word = word == "MAS" || word == "SAM"

