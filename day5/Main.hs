import Data.List.Split

main :: IO ()
main = do
    example

type Page = Int
type PageOrder = [Int]
type Rule = (Page, Page)
type RuleMap = [(Page, [Page])]

example :: IO ()
example = do
    content <- readFile "input-test.txt"
    let res = parseFileInput content
    -- print res
    res2 <- lines <$> readFile "input-test.txt"
    print res2
    print $ splitWhen (all (`elem` " \t\n")) res2
    -- let [ruleLines,updateLines] = splitWhen (== " ") inputLines
    -- print res
    -- in case res of
    -- mapM_ print ruleLines
   
    return ()

foo :: IO ()
foo = do
    print "foobar"
    return ()

parseFileInput :: String -> (RuleMap, [PageOrder])
parseFileInput str
    | length res < 2        = error "Unexpected input"
    | otherwise             = ([], [])
    where
        xsLines = lines str
        res = splitWhen (=="\n") xsLines


-- getFileInput :: String -> IO (RuleMap, PageOrder)
-- getFileInput filename
--     | any length    [ruleLines, updateLines]    = ([],[])
--     | otherwise = ([], [])
--     where
--         content <- lines <$> readFile filename
--         res@[ruleLines,updateLines] = map (map readInts) $ splitWhen (==" ") content
--         -- res =  $ splitWhen (==" ") content
--         print res
--         return ([], [])

readInts :: String -> [Int]
readInts = map read . words
-- mkRules :: [String] -> RuleMap
-- mkRules = map ((\[a,b] -> (read a, map read b)) . words)


-- just need to mk the rule map...
validPageOrder :: RuleMap -> [Page] -> Bool
validPageOrder rules pages = False
-- validPageOrder rules pages

-- passesRule :: -> Rule -> [Page] -> Bool
-- passesRule _ [] = True
-- passesRule (a,b) ps = False

middlePage :: [Page] -> Page
middlePage [x] = x
middlePage [x,y] = (x * y) `div` 2
middlePage (x:xs) = middlePage $ dropLast xs

dropLast :: [a] -> [a]
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

getRules :: RuleMap -> Page -> [Page]
getRules rm p =
    let rules = lookup p rm
    in case rules of
        Just ps -> ps
        Nothing -> []
