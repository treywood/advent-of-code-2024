module Main where

import Aoc
import Text.Megaparsec
import Text.Megaparsec.Char

data Dir = ASC | DESC deriving (Eq, Show)

main :: IO ()
main =
    run
        Solution
            { parser = sepBy1 (sepBy1 integer (char ' ')) newline
            , part1 = showStrLn . p1
            , part2 = showStrLn . p2
            }
  where
    p1 levels = sum $ map (toInt . isValidNoSkips) levels
    p2 levels = sum $ map (canMakeValid 1) levels

    isValidNoSkips :: [Int] -> Bool
    isValidNoSkips [] = True
    isValidNoSkips [_] = True
    isValidNoSkips (a : b : xs)
        | dist a b > 3 = False
        | a < b = validateAsc (b : xs)
        | a > b = validateDesc (b : xs)
        | otherwise = False -- Equal numbers are invalid
    validateAsc :: [Int] -> Bool
    validateAsc [] = True
    validateAsc [_] = True
    validateAsc (a : b : xs)
        | a >= b || dist a b > 3 = False
        | otherwise = validateAsc (b : xs)

    validateDesc :: [Int] -> Bool
    validateDesc [] = True
    validateDesc [_] = True
    validateDesc (a : b : xs)
        | a <= b || dist a b > 3 = False
        | otherwise = validateDesc (b : xs)

    canMakeValid :: Int -> [Int] -> Int
    canMakeValid skips xs
        | isValidNoSkips xs = 1
        | skips > 0 = if any isValidNoSkips (allVariations xs) then 1 else 0
        | otherwise = 0

    -- Try removing each number one at a time
    allVariations :: [Int] -> [[Int]]
    allVariations xs = [removeAt i xs | i <- [0 .. length xs - 1]]

    removeAt :: Int -> [Int] -> [Int]
    removeAt i xs = take i xs ++ drop (i + 1) xs

    toInt :: Bool -> Int
    toInt True = 1
    toInt False = 0

    dist a b = abs (a - b)
