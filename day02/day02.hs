import Text.Printf

parseInput :: [Char] -> [[Int]]
parseInput = map (map read . words) . lines

listIsSafe :: [Int] -> Int
listIsSafe x 
    | moreThan3 l = 0
    | notAllDecrIncr x = 0
    | otherwise = 1
  where
    l = zipWith (-) (tail x) x
    moreThan3 x = any (> 3) x || any (< -3) x
    notAllDecrIncr x = not $ (all (uncurry (<)) (zip x (tail x)))
                          || (all (uncurry (>)) (zip x (tail x)))

solvePt1 :: [[Int]] -> Int
solvePt1 xs = sum (map listIsSafe xs)

main :: IO()
main = do
  input <- parseInput <$> readFile "input.in"
  printf "Part 1: %d\n" (solvePt1 input)
