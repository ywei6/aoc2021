module Main where

import Debug.Trace

data Bit = Zero | One deriving (Show, Eq)
type Row = [Bit]
type Report = [Row]

inputFromFile :: FilePath -> IO Report
inputFromFile fpath = map (map conv) . lines <$> readFile fpath
  where conv '0' = Zero
        conv '1' = One
        conv _   = undefined -- should not reach here

cols :: Report -> Report
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

step :: (Int, Int) -> Bit -> (Int, Int)
step (o, z) Zero = (o + 1, z)
step (o, z) One = (o, z + 1)

getStats :: Report -> [(Int, Int)]
getStats = map (foldl step (0, 0)) . cols

getComp :: (Int -> Int -> Bool) -> Bit -> (Int, Int) -> Bit
getComp f z (x, y)
  | f x y = Zero
  | x == y = z
  | otherwise = One

toBinary :: Bit -> Int
toBinary Zero = 0
toBinary One = 1

toDecimal :: [Bit] -> Int
toDecimal xs = foldl (\a (i, d) -> a + toBinary i * 2^d) 0
            $ zip (reverse xs) [0..]

filterRow :: (Int -> Int -> Bool) -> Bit -> Int -> Report -> [Row]
filterRow _ _ _ r@[xs] = r
filterRow f z i r@(xs:xss) =
  filterRow f z (i + 1) $ filter (\xs' -> xs' !! i == b) r
  where b = getComp f z $ getStats r !! i

solve1 :: Report -> Int
solve1 r = g * e
  where s = getStats r
        g = toDecimal $ map (getComp (>) One) s
        e = toDecimal $ map (getComp (<) Zero) s

solve2 :: Report -> Int
solve2 r = g * e
  where i = (length . head) r - 1
        g = toDecimal $ head $ filterRow (>) One 0 r
        e = toDecimal $ head $ filterRow (<) Zero 0 r

main :: IO ()
main = do
  i <- inputFromFile "input.txt"
  print ("Part 1: " ++ show(solve1 i))
  print ("part 2: " ++ show(solve2 i))
