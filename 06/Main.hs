{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import qualified Data.Text as T (lines, splitOn, unpack)
import qualified Data.Text.IO as T (readFile)

type AgeCount = (Int, Int) -- fst is age, snd is count

type Ocean = [AgeCount]

step :: Ocean -> Ocean
step x = dedup6 (countDown x) ++ reproduce x
  where
    countDown = map (\(x, y) -> if x > 0 then (x - 1, y) else (6, y))
    reproduce = map (\(_, x) -> (8, x)) . filter ((== 0) . fst)
    dedup6 x =
      (6, sum $ map snd $ filter ((== 6) . fst) x) :
      filter ((/= 6) . fst) x

countFish :: Ocean -> Int
countFish = sum . map snd

toOcean :: [Int] -> [AgeCount]
toOcean = map f . group . sort
  where
    f x = (head x, length x)

solve :: Int -> [Int] -> Int
solve x y = sum $ map snd $ head $ drop x $ iterate step (toOcean y)

main :: IO ()
main = do
  x <-
    map (read . T.unpack) . T.splitOn "," . head . T.lines
      <$> T.readFile "input.txt"
  print (solve 80 x)
  print (solve 256 x)
