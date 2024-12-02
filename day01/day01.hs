import Data.List

groupAdjacent :: [Int] -> [[Int]]
groupAdjacent [] = []
groupAdjacent [x] = [[x]]
groupAdjacent (x:y:xs) = [x,y] : groupAdjacent xs

parseInput :: [Int] -> [[Int]]
parseInput xs = transpose $ groupAdjacent xs

solve :: [[Int]] -> Int
solve [l1,l2] = sum $ zipWith (\x y -> abs(x-y)) (sort l1) (sort l2) 

main :: IO()
main = interact $ show . solve . parseInput . map read . words

