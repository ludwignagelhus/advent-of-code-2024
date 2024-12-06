module Main where
import qualified Data.Map as Map
import Data.Map (Map, fromListWith, toList)
import Data.List (intersect, splitAt)
import Data.Maybe (fromMaybe)
-- import Control.Lens ((&))

main :: IO ()
main = do
    example

type Page = Int
type Update = [Page]
type Rule = (Page, Page)
type RuleMap = Map Page [Page]

(|>) :: a -> (a -> b) -> b
x |> f = f x

example :: IO ()
example = do
    content <- readFile "input.txt"
    let (rules, updates) = parseFileInput content

    let ruleMap = mkRuleMap rules
    print ruleMap
    mapM_ print (toList ruleMap)

    let valids = filter (okUpdate ruleMap) updates
    print valids
    print $ sum $ map (middlePage) valids
    -- print ruleMap

    return ()

parseFileInput :: String -> ([Rule], [Update])
parseFileInput content =
    let intLists         = content |> (replace ['|', ','] ' ') |> lines |> map lineToInts
        rules            = intLists |> takeWhile (not . null) |> map (\[a,b] -> (a, b))
        updates          = intLists |> dropWhile (not . null) |> drop 1
    in (rules, updates)

lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words
-- lineToInts = words |> map (read :: String -> Int)

replace :: Eq a => [a] -> a -> [a] -> [a]
replace targets with = map (\x -> if x `elem` targets then with else x)

mkRuleMap :: [Rule] -> RuleMap
mkRuleMap = fromListWith (++) . map (\(a,b) -> (a, [b]))

okUpdate :: RuleMap -> Update -> Bool
okUpdate rm ps = okUpdate' rm (reverse ps)
    where
        okUpdate' _ [] = True
        okUpdate' _ [_] = True
        okUpdate' rm (p:ps) =
            if not . null $ ps `intersect` (getRules rm p)
              then False
              else okUpdate' rm ps

getRules :: RuleMap -> Page -> [Page]
getRules rm p = fromMaybe [] (Map.lookup p rm)

middlePage :: [Page] -> Page
middlePage [x] = x
middlePage [x,y] = (x * y) `div` 2
middlePage (x:xs) = middlePage $ dropLast xs

dropLast :: [a] -> [a]
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs
