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

-- multiple monadic contexts? monad stacks?
expfn :: IO ()
expfn = do
    -- map <- parseInput <$> readFile "input-test.txt"
    map <- parseInput <$> readFile "input.txt"
    let guard = findGuard map
    case guard of
        Nothing -> error "No guard found"
        Just g -> do
            let path = walk map g
            mapM_ putStrLn map
            print path
            print (length $ nub path)

    -- let path = walk map guard
    -- print path

type Coordinate = (Int, Int)
type AreaMap = [[Char]]
data Direction = North | South | West | East deriving (Eq, Show)
type Guard = (Coordinate, Direction)

parseInput :: String -> AreaMap
parseInput str =
    let xsLines = (lines str) :: [[Char]]
        result = xsLines
    in result

findGuard :: AreaMap -> Maybe Guard
findGuard rs = do
    row <- findIndex (elem '^') rs
    col <- elemIndex '^' (rs !! row)
    return ((row, col), North)

-- state monad possible here?
type Path = [Coordinate]
walk :: AreaMap -> Guard -> Path
walk area (posPlayer, dir) = walk' area (posPlayer, dir) [posPlayer]

walk' :: AreaMap -> Guard -> Path -> Path
walk' area guard@(posPlayer, dir) path
    | not (visible area posNext)    = path
    | square area posNext == '#'    = walk' area (turnRight guard) path
    | otherwise                     = walk' area (posNext, dir) (posNext : path)
        where
            posNext = moveOne guard

turnRight :: Guard -> Guard
turnRight (pos, dir)
    | dir == North = (pos, East)
    | dir == East = (pos, South)
    | dir == South = (pos, West)
    | dir == West = (pos, North)

moveOne :: Guard -> Coordinate
moveOne (pos, dir) = case dir of
    North -> up pos
    East -> right pos
    South -> down pos
    West -> left pos

-- nicer way to write this?
visible :: AreaMap -> Coordinate -> Bool
visible area (x, y)
    | any (<0) [x,y]             = False
    | x >= length (head area)   = False
    | y >= length area          = False
    | otherwise                 = True

square :: AreaMap -> Coordinate -> Char
square area (r, c) = (area !! r) !! c

up (r, c) = (r - 1, c)
down (r, c) = (r + 1, c)
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)
