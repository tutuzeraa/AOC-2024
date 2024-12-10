import Text.Printf
import Data.List.Split -- You must install :/

solvePt1 :: String -> Int
solvePt1 = undefined

main :: IO()
main = do
    input <- readFile "input.in"
    printf "Part 1: %d\n" (solvePt1 input)