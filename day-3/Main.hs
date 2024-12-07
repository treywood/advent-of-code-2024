module Main where

import Aoc
import Text.Megaparsec
import Text.Megaparsec.Char

data Cmd = Do | Dont | Mul Int Int deriving (Show)

main :: IO ()
main =
    run
        Solution
            { parser = many skip *> many (cmd <* many skip)
            , part1 = showStrLn . p1
            , part2 = showStrLn . p2
            }
  where
    skip = notFollowedBy cmd *> anySingle
    mul = string "mul" *> between (char '(') (char ')') (Mul <$> integer <* char ',' <*> integer)
    do_ = string "do()" *> pure Do
    dont = string "don't()" *> pure Dont
    cmd = choice [mul, do_, dont]

    p1 = sum . map mult
    mult (Mul a b) = a * b
    mult _ = 0

    p2 :: [Cmd] -> Int
    p2 cmds = snd $ foldl process (True, 0) cmds
      where
        process :: (Bool, Int) -> Cmd -> (Bool, Int)
        process (False, acc) Do = (True, acc)
        process (False, acc) _ = (False, acc)
        process (True, acc) Dont = (False, acc)
        process (True, acc) (Mul a b) = (True, acc + a * b)
        process (True, acc) _ = (True, acc)
