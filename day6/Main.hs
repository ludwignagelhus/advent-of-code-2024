module Main where

import Data.List (elemIndex, findIndex, nub)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = do
    -- pt1
    expfn
    -- content <- readFile "input-test.txt"
    -- let map = parseInput content
    -- mapM_ (putStrLn . show) map

pt1 :: IO ()
pt1 = do
    area <- parseInput <$> readFile "input.txt"
    let guard@(origin, dir) = fromMaybe (error "No guard found.") (findGuard area)
    print $ length $ nub $ patrol area [origin] dir

-- 

type Area = [[Char]]

parseInput :: String -> Area
parseInput str = (lines str) :: Area

type Coordinate = (Int, Int)
data Direction = North | South | West | East deriving (Eq, Show, Ord)

findGuard :: Area -> Maybe (Coordinate, Direction)
findGuard area = do
    y <- findIndex (elem '^') area
    x <- elemIndex '^' (area !! y)
    return ((x, y), North)

type Path = [Coordinate]

patrol :: Area -> Path -> Direction -> Path
patrol area path dir
    | null path                     = error "No starting point."
    | not (visible area nextPos)    = path
    | nextsqCh == '#'               = patrol area path (turnRight dir) 
    | otherwise                     = patrol area (nextPos : path) dir
    where
        nextPos = moveOne (head path) dir
        nextsqCh = sqCh area nextPos

visible :: Area -> Coordinate -> Bool
visible area (x, y)
    | any (<0) [x,y]            = False
    | x >= length (head area)   = False
    | y >= length area          = False
    | otherwise                 = True

turnRight :: Direction -> Direction
turnRight dir = case dir of
    North -> East
    East -> South
    South -> West
    West -> North

moveOne :: Coordinate -> Direction -> Coordinate
moveOne (x, y) dir = case dir of
    North -> (x, y - 1)
    East -> (x + 1, y)
    South -> (x, y + 1)
    West -> (x - 1, y)

sqCh :: Area -> Coordinate -> Char
sqCh area (x, y) = (area !! y) !! x

--

pt2 :: IO ()
pt2 = do
    area <- parseInput <$> readFile "input.txt"
    let guard = fromMaybe (error "No guard found.") (findGuard area)
    let (origin, dir) = guard
    return ()

-- check points in path

expfn :: IO ()
expfn = do
    area <- parseInput <$> readFile "input-test.txt"
    -- area <- parseInput <$> readFile "input.txt"
    let guard@(origin,dir) = fromMaybe (error "No guard found.") (findGuard area)
    
    -- let areaWithLoop = addObstacle area (7,9)

    let res3 = filter (/=origin) $ nub $ findLoops area guard (Set.fromList [guard])
    print res3
    print $ length res3

    return ()

type Vector = (Coordinate, Direction)

findLoops :: Area -> Vector -> Set.Set Vector -> [Coordinate]
findLoops area guard@(xy,dir) seen
    | not (visible area xyNext)     = []
    | sqCh area xyNext == '#'       = findLoops area (xy, turnRight dir) (Set.insert (xy, turnRight dir) seen)
    | looping                       = xyNext : (findLoops area (xyNext, dir) (Set.fromList [(xyNext, dir)]))
    | otherwise                     = findLoops area (xyNext, dir) (Set.fromList [(xyNext, dir)])
    where
        xyNext = moveOne xy dir
        looping = isLooping (addObstacle area xyNext) (xy, turnRight dir) (Set.insert (xy, turnRight dir) seen)
    
isLooping :: Area -> Vector -> Set.Set Vector -> Bool
isLooping area guard@(xy,dir) seen
    | not (visible area xyNext)          = False
    | Set.member (xyNext,dir) seen       = True
    | sqCh area xyNext == '#'            = isLooping area (xy, turnRight dir) (Set.insert (xy, turnRight dir) seen)
    | otherwise                          = isLooping area (xyNext, dir) (Set.insert (xyNext, dir) seen)
    where
        xyNext = moveOne xy dir

addObstacle :: Area -> Coordinate -> Area
addObstacle area (x, y) = take y area ++ [take x (area !! y) ++ ['#'] ++ drop (x + 1) (area !! y)] ++ drop (y + 1) area

printArea :: Area -> IO ()
printArea area = putStrLn $ unlines area

findObstacles :: String -> Int -> (Int, Int) -> [Coordinate]
findObstacles [] _ _ = []
findObstacles (c:chs) lineLength (x, y)
    | c == '#'  = (x, y) : findObstacles chs lineLength nextPos
    | otherwise = findObstacles chs lineLength nextPos
  where
    nextPos = if x + 1 == lineLength then (0, y + 1) else (x + 1, y)
    