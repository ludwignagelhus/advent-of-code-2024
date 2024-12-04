module Main where
import Data.List (permutations)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Util

main :: IO ()
main = do
    pt1
    pt2

pt1 :: IO ()
pt1 = do
    puzzle <- readInput "input.txt"
    let nRows   = length puzzle; nCols = length (head puzzle)
        coords  = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]
        total   = sum . map (countXmas puzzle) $ coords
    print total

type Puzzle = [String]
readInput :: String -> IO Puzzle
readInput fileName = lines <$> readFile fileName

countXmas :: Puzzle -> Coord -> Int
countXmas puzzle orig =
    let east        = mapMaybe (getPChar puzzle . addCoords orig) [(0,0),  (1,0),  (2,0),  (3,0)]
        southEast   = mapMaybe (getPChar puzzle . addCoords orig) [(0,0),  (1,1),  (2,2),  (3,3)]
        south       = mapMaybe (getPChar puzzle . addCoords orig) [(0,0),  (0,1),  (0,2),  (0,3)]
        southWest   = mapMaybe (getPChar puzzle . addCoords orig) [(0,0), (-1,1), (-2,2), (-3,3)]
    in
       length $ filter (`elem` ["XMAS", "SAMX"]) [east, southEast, south, southWest]

pt2 :: IO ()
pt2 = do
    puzzle <- readInput "input.txt"
    let nRows   = length puzzle; nCols = length (head puzzle)
        coords  = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]
        total   = length . filter (isMASAM puzzle) $ coords
    print total

isMASAM :: Puzzle -> Coord -> Bool
isMASAM puzzle (x,y)
    | length pChars /= 5                                      = False
    | all (`elem` ["MAS", "SAM"]) [[nw,orig,se],[sw,orig,ne]] = True
    | otherwise                                               = False
    where
        pChars = getXCoords puzzle (x,y) |> mapMaybe (getPChar puzzle) 
        [nw, ne, orig, sw, se] = pChars

getXCoords :: Puzzle -> Coord -> [Coord]
getXCoords puzzle (x,y) = [(x-1, y-1), (x+1,y-1), (x,y), (x-1,y+1), (x+1,y+1)]

getPChar :: Puzzle -> Coord -> Maybe Char
getPChar puzzle (x,y) =
    if x < 0 || y < 0 || y >= length puzzle || x >= length (puzzle !! y)
        then Nothing
   else Just (puzzle !! y !! x)

