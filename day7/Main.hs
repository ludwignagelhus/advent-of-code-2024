module Main where

-- import Data.List (elemIndex, findIndex, nub)
import Data.List.Split (splitWhen)
-- import Data.Maybe (fromMaybe)
-- import qualified Data.Set as Set

(|>) :: a -> (a -> b) -> b
x |> f = f x

main :: IO ()
main = do
    putStrLn "todo"

expfn :: IO ()
expfn = do
    putStrLn "todo"

pt1 :: IO ()
pt1 = do
    content <- readFile "input.txt"
    let equations = map parseEquation $ lines content
    let result = (sum . map fst . filter solvable) equations
    print result

ptTest :: IO ()
ptTest = do
    content <- readFile "input-test.txt"
    let equations = map parseEquation $ lines content
    let result = (sum . map fst . filter solvable) equations
    print result
    return ()

parseEquation :: String -> (Int, [Int])
parseEquation str
    | length parts < 2   = error "Invalid input."
    | otherwise          = (expected, numbers)
    where
        parts = splitWhen (== ':') str
        expected   = (read :: String -> Int) $ head parts
        numbers    = map (read :: String -> Int) $ words $ parts !! 1

-- parseInput :: String -> [(Int, [Int])]
-- parseInput [str, strs] = parseInput [str] : parseInput strs
-- parseInput [str]
--     | length parts < 2 =      error "Invalid input."
-- --     -- | length numbers == 2 =   error "Invalid input."
-- --     -- | otherwise = (expected, numbers)
--     | otherwise = (expected, [3,4])
--     where
--         parts = splitWhen (== ":") str
-- --         parseLine :: String -> (Int, [Int])
--         expected   = (read :: String -> Int) $ head parts
--         numbers    = map (read :: String -> Int) $ words $ tail parts

type Equation = (Int, [Int])

solvable :: Equation -> Bool
solvable (z, [])       = error "Invalid input."  
solvable (z, [x])      = x == z
solvable (z, x:y:xs) = solvable (z, x + y : xs) || solvable (z, x * y : xs)
