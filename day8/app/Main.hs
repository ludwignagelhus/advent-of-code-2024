module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List ( permutations )
import Data.Set (fromList)

main :: IO ()
main = do
    putStrLn "main"
    erf <- hspec spec
    return ()

pt1 :: IO ()
pt1 = do
    content <- readFile "input.txt"
    putStrLn content
    putStrLn "pt1"

-- antenna   -> [A-Z,a-z,0-9]
-- blank     -> .

-- anti node -> #

-- goal: unique antinode locations.

add :: Point -> Point -> Point
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

type Point = (Int, Int)
type Frequency = Char
type Antenna = (Point, Frequency)
type Map = [[Char]]

findAntinodes :: Antenna -> Antenna -> [Point]
-- findAntinodes (p1, hz1) (p2, hz2) = []
findAntinodes (p1, hz1) (p2, hz2)
    | hz1 /= hz2    = error "antennas must have the same frequency"
    | otherwise     = [add p1 diff, sub p2 diff]
    where
        diff = sub p1 p2

-- findInMap :: Map -> Bool
-- findInMap map = False 
    -- let antennas = [nullPoint, nullPoint]
    -- in False
-- isVisible :: Map -> Point -> Bool
-- isVisible map (x, y) = 
--     let
--     in True

-- don't need this nonsense when have unit testing?

fnexp :: IO ()
fnexp = do
    putStrLn "exp"
    content <- readFile "input-test.txt"
    putStrLn content
    print $ findAntinodes ((4, 3), 'A') ((5, 5), 'A')
    print $ findAntinodes ((5, 5), 'A') ((4, 3), 'A')


spec :: Spec
spec = do
  describe "utils for day 8" $ do
    it "parses test input correctly" $ do
        readContent <- readFile "../input-test.txt"
        putStrLn readContent
        let antennas = [((4, 3), 'A'), ((5, 5), 'A')]
            expectedAntennas = fromList [((6, 7), 'A'), ((3, 1), 'A')]
        42 `shouldBe` 42
      
    it "finds antinodes for antenna pair" $ do
      findAntinodes ((4, 3), 'A') ((5, 5), 'A') `shouldSatisfy` (`elem` permutations [(6, 7), (3, 1)])
