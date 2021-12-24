module Main where

import Data.Either (lefts, rights)
import Data.List (lines, sort)

type Parsed = Either Char String

points1 :: Char -> Int
points1 ')' = 3
points1 ']' = 57
points1 '}' = 1197
points1 '>' = 25137
points1 _ = 0 -- should not reach here

points2 :: String -> Int
points2 = foldl (\acc x -> acc * 5 + x) 0 . map point
  where
    point '(' = 1
    point '[' = 2
    point '{' = 3
    point '<' = 4
    point _ = 0 -- should not reach here

parse :: String -> Parsed
parse = parse_ []
  where
    parse_ xs [] = Right xs
    parse_ (x : xs) (c : cs) =
      if closeParen c
        then
          if matchParen x c
            then parse_ xs cs
            else Left c
        else parse_ (c : x : xs) cs
    parse_ [] (c : cs) = parse_ [c] cs

matchParen :: Char -> Char -> Bool
matchParen '(' ')' = True
matchParen '[' ']' = True
matchParen '{' '}' = True
matchParen '<' '>' = True
matchParen _ _ = False

closeParen :: Char -> Bool
closeParen = flip elem [')', ']', '}', '>']

solve1 :: [String] -> Int
solve1 = sum . map points1 . lefts . map parse

solve2 :: [String] -> Int
solve2 xs =
  let ys = sort . map points2 . rights $ map parse xs
   in ys !! (length ys `div` 2)

fromInput :: FilePath -> IO [String]
fromInput s = lines <$> readFile s

main :: IO ()
main = do
  xs <- fromInput "input.txt"
  print ("Part 1: " ++ show (solve1 xs))
  print ("Part 2: " ++ show (solve2 xs))
