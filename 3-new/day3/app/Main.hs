-- module Main (main) where

import Text.Regex.TDFA
import Text.Read (readMaybe)

main :: IO ()
main = do
    let input = "Here are some numbers: 12, 123, 1234, 5"
        pattern = "\\b[0-9]{1,3}\\b"  -- Match 1 to 3 digits
        matches = getAllTextMatches (input =~ pattern)
        -- intMatches = [read m | m <- matches]
        -- intMatches = [m | Just m <- map readMaybe matches]
        intMatches = map read matches :: [Int]

    print intMatches


-- mulString :: String -> Int
-- mulString s =
--     let n1 = read (s =~ "[0-9]3+" :: String)
--     let n2 = read (s =~ "[0-9]3+" :: String)
--     in n1 * n2

-- readInput :: String -> IO String
-- readInput filename = do
--     input <- readFile filename
--     return  (head $ lines input)
