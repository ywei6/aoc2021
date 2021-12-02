module Main where

data Verb = F | D | U deriving (Show, Eq)

type Command = (Verb, Int)

parseLine :: String -> Command
parseLine l =
  let [v, s] = take 2 $ words l
      v' = case v of
        "up" -> U
        "down" -> D
        _ -> F
   in (v', read s)

inputFromFile :: FilePath -> IO [Command]
inputFromFile filePath = map parseLine . lines <$> readFile filePath

step2 :: ((Int, Int), Int) -> Command -> ((Int, Int), Int)
step2 ((h, d), a) (F, i) = ((h + i, d + a * i), a)
step2 (s, a) (D, i) = (s, a + i)
step2 (s, a) (U, i) = (s, a - i)

solve2 :: [Command] -> Int
solve2 l =
  let ((h, d), _) = foldl step2 ((0, 0), 0) l
  in h * d

step1 :: (Int, Int) -> Command -> (Int, Int)
step1 (h, d) (F, i) = (h + i, d)
step1 (h, d) (D, i) = (h, d + i)
step1 (h, d) (U, i) = (h, d - i)

solve1 :: [Command] -> Int
solve1 l =
  let (h, d) = foldl step1 (0, 0) l
  in h * d

main :: IO ()
main = do
  i <- inputFromFile "input.txt"
  print ("Part 1: " ++ show (solve1 i))
  print ("Part 2: " ++ show (solve2 i))
