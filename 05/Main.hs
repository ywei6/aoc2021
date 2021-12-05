{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M (Map, empty, filter, insertWith, size, findWithDefault)
import qualified Data.Text as T (Text, splitOn, lines, unpack)
import qualified Data.Text.IO as T (readFile)

type Point = (Int, Int)
type Line = (Point, Point)
type Vent = M.Map Point Int

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) =
  abs(x1 - x2) `mod` abs(y1 - y2) == 0

ramp :: Int -> Int -> [Int]
ramp x1 x2
  | x1 <= x2  = [x1..x2]
  | otherwise = reverse [x2..x1]

getPoints :: Line -> [Point]
getPoints ((x1, y1), (x2, y2))
  | x1 == x2 = map (x1, ) (ramp y1 y2)
  | y1 == y2 = map (, y1) (ramp x1 x2)
  | abs(y1 - y2) == abs(x1 - x2) = zip (ramp x1 x2) (ramp y1 y2) --comment this out to get the answer of Part 1
  | otherwise = []

runLines :: [Line] -> Vent
runLines = foldl f M.empty
  where f a x = foldl g a (getPoints x)
        g a x = M.insertWith (+) x 1 a

countOver :: Vent -> Int
countOver = M.size . M.filter (>= 2)

toString :: Int -> Int -> Vent -> String
toString x y z =
  concat [show (M.findWithDefault 0 (j, i) z) ++
            (if j == y - 1 then "\n" else ",") |
          i <- [0..(x - 1)], j <- [0..(y - 1)]]

parseLineT :: T.Text -> Line
parseLineT = g . map f . T.splitOn " -> "
  where f = g . map (read . T.unpack) . T.splitOn ","
        g (x:y:_) = (x, y)

fromFile :: FilePath -> IO [Line]
fromFile = fmap (map parseLineT . T.lines) . T.readFile

main :: IO ()
main = do
  x <- fromFile "input.txt"
  print (show (countOver $ runLines x))
