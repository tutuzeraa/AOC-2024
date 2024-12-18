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


main :: IO()
main = do
    input <- readFile "input.in"
    printf "Part 1: %d\n" (solvePt1 input 0)