module Main where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (nub, sortBy)

-- Return the width of the height map
getWidth :: [[Int]] -> Int
getWidth = length . head

-- Return the depth of the height map
getDepth :: [[Int]] -> Int
getDepth = length

-- Return the height of a given point in the map
getHeight :: [[Int]] -> (Int, Int) -> Int
getHeight g (x, y) = (g !! (y - 1)) !! (x - 1)

-- Test if a point is in the map
inScope :: [[Int]] -> (Int, Int) -> Bool
inScope g (x, y) = x >= 1 && x <= getWidth g && y >= 1 && y <= getDepth g

-- Find all neighbors of a point, regardless if any neighbor is in the map
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- Tell if a point in a map is a low point
isLowPoint :: [[Int]] -> (Int, Int) -> Bool
isLowPoint g xy =
  let h = getHeight g xy
   in all
        ((> h) . getHeight g)
        $ filter
          (inScope g)
          (neighbors xy)

-- Find all low points in a map
lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints g =
  filter
    (isLowPoint g)
    [(x, y) | x <- [1 .. (getWidth g)], y <- [1 .. (getDepth g)]]

-- Finad all neibors heigher than the point but lower than 9
highNeighbors :: [[Int]] -> (Int, Int) -> [(Int, Int)]
highNeighbors g xy =
  let h = getHeight g xy
   in filter ((> h) . getHeight g) $
        filter ((< 9) . getHeight g) $
          filter (inScope g) $ neighbors xy

-- Find the basin around a given low point
ascent :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
ascent g [] = []
ascent g xs =
  let xs' =
        filter (`notElem` xs) $
          nub $ concatMap (highNeighbors g) xs
   in case xs' of
        [] -> xs
        _ -> ascent g (xs ++ xs')

solve1 :: [[Int]] -> Int
solve1 g = sum . map ((+ 1) . getHeight g) $ lowPoints g

solve2 :: [[Int]] -> Int
solve2 g =
  product . map length . take 3 . sortBy (on (flip compare) length)
    . map (ascent g . (: []))
    $ lowPoints g

fromInput :: FilePath -> IO [[Int]]
fromInput f = map (map digitToInt) . lines <$> readFile f

main :: IO ()
main = do
  g <- fromInput "input.txt"
  print ("Part 1: " ++ show (solve1 g))
  print ("Part 2: " ++ show (solve2 g))
