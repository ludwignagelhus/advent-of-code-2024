module Main where

import Data.List (elemIndex, findIndex, nub)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe)

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = do
    content <- readFile "input-test.txt"
    let map = parseInput content
    mapM_ (putStrLn . show) map

pt1 :: IO ()
pt1 = do
    area <- parseInput <$> readFile "input.txt"
    let guard = findGuard area
    case guard of
        Nothing -> error "No guard found."
        Just (origin, dir) -> print $ length $ nub $ patrol area [origin] dir

pt2 :: IO ()
pt2 = do
    area <- parseInput <$> readFile "input.txt"
    let guard = findGuard area
    return ()

expfn :: IO ()
expfn = do
    -- area <- parseInput <$> readFile "input-test.txt"
    pt1
    -- pt2

type AreaMap = [[Char]]

parseInput :: String -> AreaMap
parseInput str = (lines str) :: AreaMap

type Coordinate = (Int, Int)
data Direction = North | South | West | East deriving (Eq, Show)

findGuard :: AreaMap -> Maybe (Coordinate, Direction)
findGuard area = do
    y <- findIndex (elem '^') area
    x <- elemIndex '^' (area !! y)
    return ((y, x), North)

type Path = [Coordinate]

patrol :: AreaMap -> Path -> Direction -> Path
patrol area path dir
    | null path                     = error "No starting point."
    | not (visible area nextPos)    = path
    | nextSquare == '#'             = patrol area path (turnRight dir) 
    | otherwise                     = patrol area (nextPos : path) dir
    where
        nextPos = moveOne (head path) dir
        nextSquare = square area nextPos

visible :: AreaMap -> Coordinate -> Bool
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

square :: AreaMap -> Coordinate -> Char
square area (x, y) = (area !! y) !! x

