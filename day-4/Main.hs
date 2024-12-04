module Main where

import Text.Read (readMaybe)
import TestModule

main :: IO ()
main = do
    someFunc

    putStrLn "day 4"

    -- input <- readInput "input.txt"
    -- let mulStrings = getMulStringsPt2 input
    -- print $ smartExec mulStrings True


fnFromMain :: IO ()
fnFromMain = do
    putStrLn "fnFromMain"