module Main where

import Aoc
import Data.Function ((&))
import Data.List (sort)
import Data.Map (findWithDefault, insertWith)
import Text.Megaparsec
import Text.Megaparsec.Char (space)

main :: IO ()
main =
    run
        Solution
            { parser = unzip <$> (some (pair <* space))
            , part1 = showStrLn . p1
            , part2 = showStrLn . p2
            }
  where
    pair :: Parser (Int, Int)
    pair = (,) <$> integer <* space <*> integer

    p1 :: ([Int], [Int]) -> Int
    p1 (as, bs) = zipWith distance as' bs' & sum
      where
        as' = sort as
        bs' = sort bs
        distance :: Int -> Int -> Int
        distance a b = abs (a - b)

    p2 :: ([Int], [Int]) -> Int
    p2 (as, bs) = map similarity as & sum
      where
        similarity a = a * (findWithDefault 0 a freqs)
        freqs = foldr (\a -> insertWith (+) a 1) mempty bs
