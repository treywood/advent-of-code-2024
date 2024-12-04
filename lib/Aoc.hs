module Aoc where

import Data.Void (Void)
import System.IO
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, some)

type Parser = Parsec Void String
data Solution a = Solution
    { parser :: Parser a
    , part1 :: a -> IO ()
    , part2 :: a -> IO ()
    }

run :: Solution a -> IO ()
run solution = do
    inputStr <- hGetContents stdin
    case parse (parser solution) "" inputStr of
        Left bundle -> putStrLn (errorBundlePretty bundle)
        Right input -> do
            putStrLn "Part 1:\n"
            (part1 solution) input

            putStrLn "\nPart 2:\n"
            (part2 solution) input

notImplemented :: a -> IO ()
notImplemented = const $ putStrLn "Not implemented"

showStrLn :: (Show a) => a -> IO ()
showStrLn = putStrLn . show

integer :: Parser Int
integer = read <$> some (oneOf ['0' .. '9'])
