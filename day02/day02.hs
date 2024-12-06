import Text.Printf
import Data.List 

parseInput :: [Char] -> [[Int]]
parseInput = map (map read . words) . lines

listIsSafe :: [Int] -> Bool 
listIsSafe x 
    | moreThan3 l = False 
    | notAllDecrIncr x = False 
    | otherwise = True 
  where
    l = zipWith (-) (tail x) x
    moreThan3 x = any (> 3) x || any (< -3) x
    notAllDecrIncr x = not $ (all (uncurry (<)) (zip x (tail x)))
                          || (all (uncurry (>)) (zip x (tail x)))

removeOne :: [Int] -> [[Int]]
removeOne [] = []
removeOne (x:xs) = xs : map (x:) (removeOne xs)

canBeSafe :: [Int] -> Bool
canBeSafe xs
    | null xs = False
    | otherwise = any listIsSafe (removeOne xs)

solvePt1 :: [[Int]] -> Int
solvePt1 xs = length $ filter listIsSafe xs

solvePt2 :: [[Int]] -> Int
solvePt2 xs = length $ filter canBeSafe (filter (not . listIsSafe) xs)

main :: IO()
main = do
  input <- parseInput <$> readFile "input.in"
  printf "Part 1: %d\n" (solvePt1 input)
  printf "Part 2: %d\n" (solvePt1 input + solvePt2 input)
