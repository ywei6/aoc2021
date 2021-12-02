module Main where

import Data.List

solve1 :: [Int] -> Int
solve1 l = length . filter (>0) $ zipWith (-) (tail l) l

solve2 :: [Int] -> Int
solve2 l = solve1 $ map sum w
    where w = takeWhile ((==3) . length) $ map (take 3) $ tails l

inputFromFile :: FilePath -> IO [Int]
inputFromFile f = map read . lines <$> readFile f

main :: IO ()
main = do
    input <- inputFromFile "input.txt"
    print ("Part 1: " ++ show (solve1 input))
    print ("part 2: " ++ show (solve2 input))
