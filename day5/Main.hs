module Main where
import Data.List.Split
import Data.List (groupBy)
import Data.Map (Map, fromListWith)
import Control.Arrow ((>>>))

main :: IO ()
main = do
    example

type Page = Int
type PageOrder = [Int]
type Updates = [Page]
type Rule = (Page, Page)
type RuleMap = Map Page [Page]

-- read about operator... specificity?
(|>) :: a -> (a -> b) -> b
x |> f = f x

example :: IO ()
example = do
    content <- readFile "input-test.txt"
    let intLists         = content |> (replace ['|', ','] ' ') |> lines |> map lineToInts
        rules            = intLists |> takeWhile (not . null) |> map (\[a,b] -> (a, b))
        updates          = intLists |> (drop 1 . dropWhile (not . null))

    mapM_ print rules
    mapM_ print updates

    -- let ruleMap = mkRuleMap rules
    -- print ruleMap

    -- case res of
    --     [rules, updates] -> print rules
    --     _ -> error "Unexpected input"

lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words

replace :: Eq a => [a] -> a -> [a] -> [a]
replace targets with = map (\x -> if x `elem` targets then with else x)

mkRuleMap :: [Rule] -> RuleMap
mkRuleMap = fromListWith (++) . map (\(a,b) -> (a, [b]))

-- just need to mk the rule map...
validUpdates :: RuleMap -> Updates -> Bool
validUpdates _ [] = True
validUpdates rules (x:xs) = False
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

-- getRules :: RuleMap -> Page -> [Page]
-- getRules rm p =
--     let rules = lookup p rm
--     in case rules of
--         Just ps -> ps
--         Nothing -> []
