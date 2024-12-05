import Data.Maybe (mapMaybe, catMaybes)

main :: IO ()
main = pt1 >> pt2

type Puzzle = [[Char]]
type Coord = (Int, Int)

pt1 :: IO ()
pt1 = do
    puzzle <- lines <$> readFile "input.txt"
    let nRows   = length puzzle; nCols = length (head puzzle)
        coords  = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]
        total   = sum . map (countXmas puzzle) $ coords
    print total

countXmas :: Puzzle -> Coord -> Int
countXmas puzzle (px,py) =
    let localGetPChars = mapMaybe (getPChar puzzle . \(x,y) -> (px+x, py+y))
        east        = localGetPChars [(0,0),  (1,0),  (2,0),  (3,0)]
        southEast   = localGetPChars [(0,0),  (1,1),  (2,2),  (3,3)]
        south       = localGetPChars [(0,0),  (0,1),  (0,2),  (0,3)]
        southWest   = localGetPChars [(0,0), (-1,1), (-2,2), (-3,3)]
    in
       length . filter (`elem` ["XMAS", "SAMX"]) $ [east, southEast, south, southWest] 

getPChar :: Puzzle -> Coord -> Maybe Char
getPChar puzzle (x,y) =
    if x < 0 || y < 0 || y >= length puzzle || x >= length (puzzle !! y)
        then Nothing
    else Just (puzzle !! y !! x)

pt2 :: IO ()
pt2 = do
    puzzle <- lines <$> readFile "input.txt"
    let nRows   = length puzzle; nCols = length (head puzzle)
        coords  = [(x,y) | x <- [0..nCols-1], y <- [0..nRows-1]]
        total   = length . filter (isMASAM puzzle) $ coords
    print total

isMASAM :: Puzzle -> Coord -> Bool
isMASAM puzzle p
    | length chars /= 5                                     = False
    | all (`elem` ["MAS", "SAM"]) [[nw,o,se],[sw,o,ne]]     = True
    | otherwise                                             = False
    where
        chars = mapMaybe (getPChar puzzle) $ getXCoords puzzle p
        [nw, ne, o, sw, se] = chars

getXCoords :: Puzzle -> Coord -> [Coord]
getXCoords puzzle (x,y) = [(x-1, y-1), (x+1,y-1), (x,y), (x-1,y+1), (x+1,y+1)]