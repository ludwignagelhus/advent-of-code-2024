module Main where

import Lib (someFunc)
import Text.Regex.TDFA
import Text.Read (readMaybe)

hw :: IO ()
hw = putStrLn "Hello, World!"

main :: IO ()
main = do
    someFunc
    input <- readInput "input.txt"
    let mulStrings = getMulStringsPt2 input
    print $ smartExec mulStrings True

readInput :: String -> IO String
readInput filename = do
    input <- readFile filename
    return  (head $ lines input)

getMulStrings :: String -> [String]
getMulStrings s =
    let pattern = "mul\\(\\b[0-9]{1,3}\\b,\\b[0-9]{1,3}\\b\\)"
    in getAllTextMatches (s =~ pattern)

mulStr :: String -> Int
mulStr s =
    let pattern = "\\b[0-9]{1,3}\\b"
        matches = getAllTextMatches (s =~ pattern)
        (fst:snd:_) = map read matches :: [Int]
    in fst * snd

getMulStringsPt2 :: String -> [String]
getMulStringsPt2 s =
    let pattern = "mul\\(\\b[0-9]{1,3}\\b,\\b[0-9]{1,3}\\b\\)|don't\\(\\)|do\\(\\)"
        matches = getAllTextMatches (s =~ pattern)
    in matches

-- smartExec :: [String] -> Bool -> Int
-- smartExec [] _ = 0
smartExec [x] exec = if exec then mulStr x else 0
smartExec (x:xs) exec
    | x == "do()"                       = smartExec xs True
    | x == "don't()" || exec == False   = smartExec xs False
    | exec == True                      = mulStr x + smartExec xs True
    