main :: IO ()
main = do
    reports <- readInput
    let result = foldl (\ac r -> if pt2 r True then ac+1 else ac)
                        0 reports
    print result

    -- let result = pt2 [1,2,3,4] True
    -- print result
    -- print $ pt2 [1,4,2] True
    -- print $ pt2 [1,4,2,4] True
    -- print $ pt2 [9,4,2,4] True
    -- print $ pt2 [9,4,4] True
    -- print $ pt2 [9,4,3] True
    -- print $ pt2 [20,15,18,12,15] True


-- 0,1,2
-- 0,1,3
-- 0,2,3
-- 1,2,3
-- three nums and then 'rest'


pt2 :: [Int] -> Bool -> Bool
pt2 [] _ = True -- erf
pt2 [x] _ = True -- erf
pt2 [x,y] canSkip = if canSkip then True else (abs (x - y)) <= 3
pt2 [x,y,z] canSkip
    | canSkip == False = checkThree [x,y,z]
    | abs (x - z) <= 3  = True
pt2 [x,y,z,zz] canSkip =
    if canSkip == False then
        checkThree [x,y,z]
    else
        pt2 [y,z,zz] False
        || pt2 [x,y,z] False
        || pt2 [y,z,zz] False

pt2 (x:y:z,zz xs) canSkip
    | canSkip == False      = checkThree

    let x3check = checkThree [x,y,head xs]
    if x3check then True
    else
        pt2 [y,z] False
    let xyDiff = x - y
    let yzDiff = y - z
    let validDiffs = abs xyDiff <= 3 && abs yzDiff <= 3 && xyDiff * yzDiff > 0
    

pt2 (x:y:z:xs) canSkip =
    let xyDiff = x - y
    let yzDiff = y - z
    let combinedValid = xyDiff * yzDiff > 0

pt2 (x:y:xs) canSkip =
    let validDiff = abs (x - y) <= 3 && x /= y
    in case (validDiff, canSkip) of
        (False, False) -> False
        (True, _) -> pt2 (y:xs) canSkip
        (False, True) -> pt2 (x:xs) False || pt2 (y:xs) False

-- erf

checkThree :: [Int] -> Bool
checkThree [x,y,z] =
    let xyDiff = x - y
    let yzDiff = y - z
    in abs xyDiff <= 3 && abs yzDiff <= 3 && xyDiff * yzDiff > 0
    

-- readInput2 :: IO [[Int]]
-- readInput2 = do
--     return [[1,2,3,4]
--         ]

readInput :: IO [[Int]]
readInput = do
    fileLines <- lines <$> readFile "input.txt"
    let reports = map lineToInts fileLines
    return reports
    
lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words


