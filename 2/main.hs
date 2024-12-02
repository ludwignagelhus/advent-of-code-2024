main :: IO ()
main = do
    reports <- readInput
    -- map_
    print $ length reports
    let result = foldl (\ac r -> if checkerRec r Unset then ac+1 else ac)
                        0 reports
    print result
    -- let res = checkerRec [1, 2, 3, 4] Unset
    -- print res

data LevelDir = Asc | Des | Unset
    deriving (Eq, Show)

checkerRec :: [Int] -> LevelDir -> Bool
checkerRec [_] _ = True
checkerRec [] _ = True
checkerRec (lv1 : lv2 : lvls) dir
    | lv1 == lv2                = False
    | abs (lv1 - lv2) > 3       = False 
    | lv1 > lv2 && dir == Asc   = False
    | lv1 < lv2 && dir == Des   = False
    | lv1 > lv2                 = checkerRec (lv2:lvls) Des
    | lv1 < lv2                 = checkerRec (lv2:lvls) Asc
    -- | otherwise                 = checkerRec (lv2:lvls) dir

-- main = do
--     lines <- lines <$> readFile "input.txt"
--     let reports = map (map (read :: String -> Int) . words) lines
--     reports

readInput2 :: IO [[Int]]
readInput2 = do
    -- fileLines <- lines <$> readFile "input.txt"
    -- let reports = map lineToInts fileLines
    return [
        [1,2,3,4]
        ,[1,2,4,5]
        ,[2,4,5]
        ,[5,4,2]
        -- [1,2,3,4],
        -- [1,2,1,2],
        -- [1,2,3,4]
        ]

readInput :: IO [[Int]]
readInput = do
    fileLines <- lines <$> readFile "input.txt"
    let reports = map lineToInts fileLines
    return reports
    
lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words


