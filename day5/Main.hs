
main :: IO ()
main = do
    exampleInput

readInput :: String -> IO [Int]
readInput filename = do
    content <- readFile filename
    return $ map read . words . filter (/= '|') content

exampleInput :: IO ()
exampleInput = do
    input <- readInput "input-test.txt"
    print input
