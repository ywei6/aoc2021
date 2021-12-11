module Main where

totalCost :: (Int -> Int -> Int) -> [Int] -> Int -> Int
totalCost f xs y = sum $ map (f y) xs

constDist :: Int -> Int ->  Int
constDist x1 x0 = abs (x1 - x0)

increDist :: Int -> Int -> Int
increDist x1 x0 = sum $ take (abs (x1 - x0)) [1..]

solve1 :: [Int] -> Int
solve1 xs = minimum $ map (totalCost constDist xs) [x1..x2]
  where
    x1 = minimum xs
    x2 = maximum xs

solve2 :: [Int] -> Int
solve2 xs = minimum $ map (totalCost increDist xs) [x1..x2]
  where
    x1 = minimum xs
    x2 = maximum xs

main :: IO ()
main = undefined
