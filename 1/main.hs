import Data.List (sort)
-- import Data.List.Split (splitOn)

main :: IO ()

main = do
    linesFromFile <- lines <$> readFile "input.txt"
    let tuples = map words linesFromFile
    let nTuples = map (\l -> (read (head l) :: Int, read (last l) :: Int)) tuples
    let (firsts, seconds) = unzip nTuples
    print $ similarityScore (firsts, seconds)

    -- let sortedFirsts = sort firsts
    -- let sortedSeconds = sort seconds
    -- let zipped = zip sortedFirsts sortedSeconds
    -- let distances = map (\(x,y) -> abs $ x - y) zipped
    -- let sumDistances = sum distances
    -- putStrLn $ show sumDistances

similarityScore :: ([Int],[Int]) -> Int
similarityScore ([], ys) = 0
similarityScore (x:xs, ys) =
    let score = sum $ filter (==x) ys
    in score + similarityScore (xs,ys)
