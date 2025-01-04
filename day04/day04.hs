import Text.Printf
import Data.List
import Data.Maybe

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
countDiagonals s = diag + reverseDiag
    where
        diag = sum $ map countWords $ diagonals s
        reverseDiag = sum $ map countWords $ diagonals (map reverse s)

diagonals :: [String] -> [String]
diagonals s =
    map catMaybes . transpose $
      [replicate i Nothing ++ map Just row | (row, i) <- zip s [0..] ]

solvePt1 :: [String] -> Int
solvePt1 s = res 
    where 
        res = countHorizontal s + countVertical s + countDiagonals s 

main :: IO()
main = do
    input <- readFile "input.in" 
    let grid = lines input
        pt1 = solvePt1 grid
    printf "Part 1: %d\n" pt1