module Main where
import qualified Data.Map as Map
import Data.Map (Map, fromListWith, toList)
import Data.List (intersect, splitAt)
import Data.Maybe (fromMaybe)
-- import Control.Lens ((&))

main :: IO ()
main = do
    content <- readFile "input-test.txt"
    let (rules, updates) = parseFileInput content

    let ruleMap = mkRuleMap rules
    mapM_ print $ toList ruleMap

    let valids = filter (not . okUpdate ruleMap) updates
    print $ valids
    print $ map (fixUpdate2 ruleMap) valids

    -- print $ fixUpdate2 ruleMap [75,97,47,61,53]

    print $ sum $ map (middlePage) $ map (fixUpdate2 ruleMap) valids

type Page = Int
type Update = [Page]
type Rule = (Page, Page)
type RuleMap = Map Page [Page]

(|>) :: a -> (a -> b) -> b
x |> f = f x

parseFileInput :: String -> ([Rule], [Update])
parseFileInput content =
    let intLists         = content |> (replace ['|', ','] ' ') |> lines |> map lineToInts
        rules            = intLists |> takeWhile (not . null) |> map (\[a,b] -> (a, b))
        updates          = intLists |> dropWhile (not . null) |> drop 1
    in (rules, updates)

lineToInts :: String -> [Int]
lineToInts = map (read :: String -> Int) . words

replace :: Eq a => [a] -> a -> [a] -> [a]
replace targets with = map (\x -> if x `elem` targets then with else x)

mkRuleMap :: [Rule] -> RuleMap
mkRuleMap = fromListWith (++) . map (\(a,b) -> (a, [b]))

okUpdate :: RuleMap -> Update -> Bool
okUpdate rm ps = okUpdate' rm (reverse ps)
    where okUpdate' _ [] = True
          okUpdate' _ [_] = True
          okUpdate' rm (p:ps) =
            if not . null $ ps `intersect` (getRules rm p)
              then False
              else okUpdate' rm ps

getRules :: RuleMap -> Page -> [Page]
getRules rm p = fromMaybe [] (Map.lookup p rm)

middlePage :: [Page] -> Page
middlePage [] = error "no pages; empty list"
middlePage [x] = x
middlePage [x,y] = (x * y) `div` 2
middlePage (x:xs) = middlePage $ dropLast xs

dropLast :: [a] -> [a]
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

-- pt2
fixUpdate :: RuleMap -> Update -> Update
fixUpdate rm ps = reverse $ fixUpdate' rm (reverse ps)
    where fixUpdate' _ [] = []
          fixUpdate' _ [p] = [p]
          fixUpdate' rm (p:ps) =
            let rules = getRules rm p
                overlaps = ps `intersect` rules
                updated = if length overlaps == 0 then p : ps else p : tail ps
            in fixUpdate' rm ps

fixUpdate2 :: RuleMap -> Update -> Update
fixUpdate2 rm ps = reverse $ fixUpdate2' rm (reverse ps)
    where
        fixUpdate2' _ [] = []
        fixUpdate2' _ [p] = [p]
        fixUpdate2' rm (p:ps) =
            let rules = getRules rm p
                overlaps = ps `intersect` rules
            in if null overlaps
                then p : fixUpdate2' rm ps
                else head ps : fixUpdate2' rm (p : (drop 1 ps))
