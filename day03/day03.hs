import Text.Printf
import Data.List
import Data.Maybe
import Data.Char

tryParseNumber :: String -> Maybe (Int, String)
tryParseNumber s =
    case span isDigit s of
        ("", rest) -> Nothing
        (n, rest) -> Just (read n, rest)

solvePt1 :: String -> Int -> Int
solvePt1 [] res = res
solvePt1 s@(_:xs) res 
    | "mul(" `isPrefixOf` s = solvePt1 xs res + tryMul
    | otherwise = solvePt1 xs res
    where
        tryMul =
            fromMaybe 0 $ do
            (n1, ',':xs1) <- tryParseNumber (drop 4 s)
            (n2, ')':xs2) <- tryParseNumber xs1
            return (n1 * n2) 

solvePt2 :: String -> Bool -> Int -> Int
solvePt2 [] bool res = res
solvePt2 s@(_:xs) bool res 
    | "do()" `isPrefixOf` s = solvePt2 xs True res
    | "don't()" `isPrefixOf` s = solvePt2 xs False res
    | "mul(" `isPrefixOf` s = if bool then solvePt2 xs bool res + tryMul else solvePt2 xs bool res
    | otherwise = solvePt2 xs bool res
    where
        tryMul =
            fromMaybe 0 $ do
            (n1, ',':xs1) <- tryParseNumber (drop 4 s)
            (n2, ')':xs2) <- tryParseNumber xs1
            return (n1 * n2) 


main :: IO()
main = do
    input <- readFile "input.in"
    printf "Part 1: %d\n" (solvePt1 input 0)
    printf "Part 2: %d\n" (solvePt2 input True 0)