main :: IO ()
main = do
    reports <- readInput
    print $ length reports
    let result = foldl (\ac r -> if checkerRec r Unset then ac+1 else ac)
                        0 reports
    print result

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



readInput2 :: IO [[Int]]
readInput2 = do
    return [
        [1,2,3,4]
        ,[1,2,4,5]
        ,[2,4,5]
        ,[5,4,2]
        ]

readInput :: IO [[Int]]
readInput = do
    fileLines <- lines <$> readFile "input.txt"
    let reports = map lineToInts fileLines
    return reports
    
lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words


