main :: IO ()
main = do
    input <- readFile "input.txt"
    print input

-- todo
parseInput :: IO [String]
parseInput = do
    input <- readFile "input.txt"
    return (lines input)
