import Data.List.Split (splitWhen)

main :: IO ()
main = do
    equations <- map parseEquation . lines <$> readFile "input.txt"
    let calibrate' = calibrate equations
    putStrLn $ "Part 1: " ++ show (calibrate' [(+), (*)])
    putStrLn $ "Part 2: " ++ show (calibrate' [(+), (*), numcat])

type Equation = (Int, [Int])

parseEquation :: String -> Equation
parseEquation str
    | length parts < 2  = error "Invalid input."
    | otherwise         = (expected, numbers)
    where
        parts       = splitWhen (== ':') str
        expected    = (read :: String -> Int) $ head parts
        numbers     = map (read :: String -> Int) (words $ parts !! 1)

type Operator = Int -> Int -> Int

calibrate :: [Equation] -> [Operator] -> Int
calibrate equations ops = sum . map fst . filter (solvable ops) $ equations

solvable :: [Operator] -> Equation -> Bool
solvable _   (z, [])     = False
solvable _   (z, [x])    = x == z
solvable ops (z, x:y:xs) = any (\op -> solvable ops (z, x `op` y : xs)) ops

numcat :: Int -> Int -> Int
numcat x y = x * 10 ^ (1 + floor (logBase 10 (fromIntegral y))) + y
