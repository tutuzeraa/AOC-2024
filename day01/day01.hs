import Data.List
import Text.Printf

groupAdjacent :: [Int] -> [[Int]]
groupAdjacent [] = []
groupAdjacent [x] = [[x]]
groupAdjacent (x:y:xs) = [x,y] : groupAdjacent xs

parseInput :: [Int] -> [[Int]]
parseInput xs = transpose $ groupAdjacent xs

solvePt1 :: [[Int]] -> Int
solvePt1 [l1,l2] = sum $ zipWith (\x y -> abs(x-y)) (sort l1) (sort l2) 

similarities :: [Int] -> [Int] -> Int -> Int
similarities (x:xs) l2 acc
  | null xs = acc
  | otherwise = similarities xs l2 (acc + x * length (filter (== x) l2))

solvePt2 :: [[Int]] -> Int
solvePt2 [l1,l2] = similarities l1 l2 0

main :: IO()
main = do
  input <- parseInput . map read . words <$> readFile "input.in"
  printf "Part 1: %d\n" (solvePt1 input)
  printf "Part 2: %d\n" (solvePt2 input)
