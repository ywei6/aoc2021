{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M (Map, empty, filter, findWithDefault, insertWith, size)
import qualified Data.Text as T (Text, lines, splitOn, unpack)
import qualified Data.Text.IO as T (readFile)

type Point = (Int, Int)

type Line = (Point, Point)

type Vent = M.Map Point Int

-- generate incrementals between two ends
ramp :: Int -> Int -> [Int]
ramp x1 x2
  | x1 <= x2 = [x1 .. x2]
  | otherwise = reverse [x2 .. x1]

-- find all points along the line
-- comment out the 3rd criterion to get the answer of Part 1
getPoints :: Line -> [Point]
getPoints ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) (ramp y1 y2)
  | y1 == y2 = map (,y1) (ramp x1 x2)
  | abs (y1 - y2) == abs (x1 - x2) = zip (ramp x1 x2) (ramp y1 y2)
  | otherwise = []

-- calculate overlapping points of the lines
runLines :: [Line] -> Vent
runLines = foldl f M.empty
  where
    f a x = foldl g a (getPoints x)
    g a x = M.insertWith (+) x 1 a

-- count overlapping points with 2+ lines
countOver :: Vent -> Int
countOver = M.size . M.filter (>= 2)

-- helpful function to print the 'vent' for debugging purpose
toString :: Int -> Int -> Vent -> String
toString x y z =
  concat
    [ show (M.findWithDefault 0 (j, i) z)
        ++ (if j == y - 1 then "\n" else ",")
      | i <- [0 .. (x - 1)],
        j <- [0 .. (y - 1)]
    ]

-- translate a string into line
parseLineT :: T.Text -> Line
parseLineT = g . map f . T.splitOn " -> "
  where
    f = g . map (read . T.unpack) . T.splitOn ","
    g (x : y : _) = (x, y)

-- parse the input file
fromFile :: FilePath -> IO [Line]
fromFile = fmap (map parseLineT . T.lines) . T.readFile

main :: IO ()
main = do
  x <- fromFile "input.txt"
  print (show (countOver $ runLines x))
