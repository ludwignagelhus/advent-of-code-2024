-- module Main where

import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text () -- Necessary for some instances

main :: IO ()
main = do
    let input = "felrjfhelrkhfeklerfrehlfkerfiuhiuherf"
        pattern = "erf"  -- The pattern to match
        matches = getAllMatches (input =~ pattern :: AllMatches [] String)
    print matches

-- Helper function to extract all matches
-- getAllMatches :: AllMatches [] String -> [String]
-- getAllMatches (AllMatches matches) = matches

-- main :: IO ()
-- main = do
--     let input = "hello123world"       -- Input string
--     let pattern = "[0-9]+"           -- Regex pattern
--     let match = input =~ pattern :: String -- Perform regex match
--     putStrLn $ "Matched: " ++ match  -- Print the match
--     -- let emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"
--     print ("my email is email@email.com" =~ "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" :: Bool)
--     -- input <- readFile "input.txt"
--     -- print input


-- mulString :: String -> Int
-- mulString s =
--     let n1 = read (s =~ "[0-9]3+" :: String)
--     let n2 = read (s =~ "[0-9]3+" :: String)
--     in n1 * n2

-- -- todo
-- parseInput :: IO [String]
-- parseInput = do
--     input <- readFile "input.txt"
--     return (lines input)
