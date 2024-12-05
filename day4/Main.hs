type Puzzle = [[Char]]
type Coord = (Int, Int)

main :: IO ()
main = do
    puzzle@(row:_) <- lines <$> readFile "input.txt"
    let coords  = [(x,y) | x <- [0..length row -1], y <- [0..length puzzle-1]]
        totalPt1   = sum . map (countXmas puzzle) $ coords
        totalPt2   = length . filter (isMasam puzzle) $ coords
    putStrLn $ "pt1: " ++ show totalPt1 ++ "\npt2: " ++ show totalPt2

countXmas :: Puzzle -> Coord -> Int
countXmas p (px,py) =
    let localPch = map (pCh p . \(x,y) -> (x + px, y + py))
        e        = localPch [(0,0),  (1,0),  (2,0), (3,0)]
        se       = localPch [(0,0),  (1,1),  (2,2),  (3,3)]
        s        = localPch [(0,0),  (0,1),  (0,2),  (0,3)]
        sw       = localPch [(0,0), (-1,1), (-2,2), (-3,3)]
    in
       length $ filter (`elem` ["XMAS", "SAMX"]) [e, se, s, sw] 

pCh :: Puzzle -> Coord -> Char
pCh p@(r:_) (x,y)
    | elem y [0..length p-1] && elem x [0..length r-1] = p !! y !! x
    | otherwise = ' '

isMasam :: Puzzle -> Coord -> Bool
isMasam p@(r:_) pCoord
    | length xchars /= 5                                    = False
    | all (`elem` ["MAS", "SAM"]) [[nw,o,se],[sw,o,ne]]     = True
    | otherwise                                             = False
    where
        xchars@[nw, ne, o, sw, se] = map (pCh p) (xCoords pCoord)

xCoords :: Coord -> [Coord]
xCoords (x,y) = [(x-1, y-1), (x+1,y-1), (x,y), (x-1,y+1), (x+1,y+1)]