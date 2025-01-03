import Text.Printf
import Data.List
-- import Data.Map

countWords :: String -> Int
countWords "" = 0
countWords s@(_:xs)
    | "XMAS" `isPrefixOf` s = 1 + countWords xs
    | reverse "XMAS" `isPrefixOf` s = 1 + countWords xs
    | otherwise = countWords xs 

countHorizontal :: [String] -> Int
countHorizontal s = sum $ map countWords s

countVertical :: [String] -> Int
countVertical s = sum $ map countWords $ transpose s

countDiagonals :: [String] -> Int
countDiagonals = undefined

solvePt1 :: [String] -> Int
solvePt1 s = res 
    where 
        res = countHorizontal s + countVertical s

main :: IO()
main = do
    input <- readFile "input.in" 
    let grid = lines input
        pt1 = solvePt1 grid
    printf "Part 1: %d\n" pt1