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

step :: (Int, Int) -> Command -> (Int, Int)
step (h, d) (F, i) = (h + i, d)
step (h, d) (D, i) = (h, d + i)
step (h, d) (U, i) = (h, d - i)

solve1 :: [Command] -> Int
solve1 l =
  let (h, d) = foldl step (0, 0) l
  in h * d

main :: IO ()
main = undefined
