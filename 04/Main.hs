{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Text as T (lines, splitOn, unpack, words)
import qualified Data.Text.IO as T (readFile)
import Debug.Trace
import Data.List

type Row a = [a]

type Matrix a = [Row a]

type Marked = Bool

type Digit = Int

type Board = Matrix (Marked, Digit)

fromFile :: FilePath -> IO (Row Digit, [Matrix Digit])
fromFile f = do
  ts <- T.splitOn "\n\n" <$> T.readFile f
  let rs = map (read . T.unpack) . T.splitOn "," $ head ts
  let bs =
        map (map (map (read . T.unpack) . T.words) . T.lines) $
          tail ts
  return (rs, bs)

getCols :: Matrix a -> Matrix a
getCols [] = [] -- nonsense
getCols [xs] = [[x] | x <- xs]
getCols (xs : xss) = zipWith (:) xs (getCols xss)

getRows :: Matrix a -> Matrix a
getRows = id

markCell :: Digit -> (Marked, Digit) -> (Marked, Digit)
markCell x (m, d) = (x == d || m, d)

markBoard :: Digit -> Board -> Board
markBoard x = map (map (markCell x))

checkRow :: Row (Marked, Digit) -> Bool
checkRow = all fst

checkBoard :: Board -> Bool
checkBoard board = r || c
  where
    r = any checkRow (getRows board)
    c = any checkRow (getCols board)

score :: Board -> Digit
score = sum . map snd . concatMap (filter (not . fst))

bingo :: Row Digit -> [Board] -> Maybe (Digit, Board)
bingo [] _ = Nothing
bingo (xs : xss) ys =
  if null ysb then bingo xss ysm else Just (xs, head ysb)
  where
    ysm = map (markBoard xs) ys
    ysb = filter checkBoard ysm

takeAfter :: Digit -> Row Digit -> Row Digit
takeAfter _ [] = []
takeAfter y (xs : xss) = if xs == y then xss else takeAfter y xss

-- main logic for `solve2`
binog :: Row Digit -> [Board] -> Maybe (Digit, Board)
binog (xs : xss) [ys] =
  if checkBoard ysm
     then Just (xs, ysm)
     else binog xss [ysm]
  where ysm = markBoard xs ys    
binog (xs : xss) ys =
  if not (null xss)
    then binog xss (ysm \\ yss)
    else if not (null yss)
         then Nothing -- one number with multiple boards
         else Just (xs, head yss) -- one number and one board
  where
    ysm = map (markBoard xs) ys
    yss = filter checkBoard ysm    
binog [] _ = Nothing

solve1 :: FilePath -> IO (Maybe Digit)
solve1 f = do
  (xs, ys) <- fromFile f
  let b = bingo xs (map (map (map (False,))) ys)
  let s = (\(d, r) -> d * score r) <$> b
  return s

solve2 :: FilePath -> IO (Maybe Int)
solve2 f = do
  (xs, ys) <- fromFile f
  let b = binog xs (map (map (map (False,))) ys)
  let s = (\(d, r) -> d * score r) <$> b
  return s

main :: IO ()
main = do
  let f = "input.txt"
  r1 <- solve1 f
  print ("Part 1: " ++ show r1)
  r2 <- solve2 f
  print ("Part 2: " ++ show r2)
