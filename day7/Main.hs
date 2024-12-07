module Main where

import Data.List.Split (splitWhen)

main :: IO ()
main = do
    equations <- map parseEquation . lines <$> readFile "input.txt"
    let pt1result = (sum . map fst . filter (solvable [(*), (+)])) equations
    putStrLn $ "Part 1: " ++ show pt1result
    let pt2result = (sum . map fst . filter (solvable [(*), (+), numcat])) equations
    putStrLn $ "Part 2: " ++ show pt2result

type Equation = (Int, [Int])

parseEquation :: String -> Equation
parseEquation str
    | length parts < 2   = error "Invalid input."
    | otherwise          = (expected, numbers)
    where
        parts     = splitWhen (== ':') str
        expected = (read :: String -> Int) $ head parts
        numbers  = map (read :: String -> Int) (words $ parts !! 1)

type Op = Int -> Int -> Int

solvable :: [Op] -> Equation -> Bool
solvable ops (z, [])  = False
solvable ops (z, [x]) = x == z
solvable ops (z, x:y:xs) = any (\op -> solvable ops (z, x `op` y : xs)) ops

numcat :: Int -> Int -> Int
numcat x y =
    x * 10 ^ (1 + floor (logBase 10 (fromIntegral y))) + y

