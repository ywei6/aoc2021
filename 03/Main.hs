module Main where

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

getComp :: (Int -> Int -> Bool) -> (Int, Int) -> Bit
getComp f (x, y) = if f x y then Zero else One

toBinary :: Bit -> Int
toBinary Zero = 0
toBinary One = 1

toDecimal :: [Bit] -> Int
toDecimal xs = foldl (\a (i, d) -> a + toBinary i * 2^d) 0
            $ zip (reverse xs) [0..]

solve1 :: Report -> Int
solve1 r = g * e
  where s = getStats r
        g = toDecimal $ map (getComp (>)) s
        e = toDecimal $ map (getComp (<)) s

main :: IO ()
main = do
  i <- inputFromFile "input.txt"
  print ("Part 1: " ++ show(solve1 i))
