{-# LANGUAGE OverloadedStrings #-}

module Other where

import Data.List (intersect, permutations, sort, sortOn)
import Data.Map.Strict (Map, disjoint, empty, singleton, union)
import qualified Data.Text as T (Text, lines, splitOn, unpack, words)
import qualified Data.Text.IO as T (readFile)

type Decoder = String

data Entry = Entry
  { input :: [String],
    output :: [String]
  }

-- The segs to lit up for any given number
segs :: Int -> [Int]
segs 0 = [0, 1, 2, 4, 5, 6]
segs 1 = [2, 5]
segs 2 = [0, 2, 3, 4, 6]
segs 3 = [0, 2, 3, 5, 6]
segs 4 = [1, 2, 3, 5]
segs 5 = [0, 1, 3, 5, 6]
segs 6 = [0, 1, 3, 4, 5, 6]
segs 7 = [0, 2, 5]
segs 8 = [0, 1, 2, 3, 4, 5, 6]
segs 9 = [0, 1, 2, 3, 5, 6]
segs _ = undefined

-- Potential numbers to try with given the length of a signal
hypo :: Int -> [Int]
hypo 2 = [1]
hypo 3 = [7]
hypo 4 = [4]
hypo 5 = [2, 3, 5]
hypo 6 = [0, 6, 9]
hypo 7 = [8]
hypo _ = undefined

patterns :: [String]
patterns = permutations ['a' .. 'g']

-- Given a pattern, decode a singal into a number
decode :: String -> Decoder -> Int
decode s p = case length s of
  2 -> 1
  3 -> 7
  4 -> 4
  7 -> 8
  _ -> head $ filter (test s p . segs) [0 .. 9]
  where
    test s p xs = sort (map (p !!) xs) == sort s

-- Generate candidate decoders given a series of signals
deduce :: [String] -> [Decoder]
deduce =
  foldl
    ( \ps s ->
        [ p | p <- ps, x <- hypo (length s), ys <- permutations $ segs x, test s p ys
        ]
    )
    patterns
    . sortOn length -- starting with the short signal to prune a lot at the beginning
  where
    test s p xs = map (p !!) xs == s

-- Step function for `solve`
solveOne :: Entry -> Int
solveOne (Entry xs ys) =
  foldl
    ( \y y' ->
        y * 10 + y'
    )
    0
    $ map
      (`decode` decoder)
      ys
  where
    decoder = head $ deduce (filter ((< 7) . length) xs)

-- Part 2; Part 1 is trivial
solve :: [Entry] -> Int
solve = sum . map solveOne

parseLine :: T.Text -> Entry
parseLine x = Entry {input = head y, output = head $ tail y}
  where
    y = map (map T.unpack . T.words) $ T.splitOn " | " x

fromInput :: FilePath -> IO [Entry]
fromInput x = map parseLine . T.lines <$> T.readFile x

main :: IO ()
main = do
  xs <- fromInput "input.txt"
  print ("Part 2: " ++ show (solve xs))
