{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day03 where


import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import Data.Function (on)
import Data.List.Split
import Data.List (foldl1')
import GHC.Stack.Types (HasCallStack)

input :: IO T.Text
input = T.readFile "input/Day03.txt"


priority :: Char -> Int
priority c
  | c <= 'z' && c >= 'a' = fromEnum c - fromEnum 'a' + 1
  | c <= 'Z' && c >= 'A' = fromEnum c - fromEnum 'A' + 27
  | otherwise = error ("invalid char: " ++ show c)


splitLine :: T.Text -> (T.Text, T.Text)
splitLine line = (left, right)
  where
    (left, right) = T.splitAt (T.length line `div` 2) line

lineDiff :: T.Text -> T.Text -> S.Set Char
lineDiff = S.intersection `on` toSet
  where
   toSet = S.fromList . T.unpack
lineDiffs :: [T.Text] -> S.Set Char
lineDiffs = foldl1' S.intersection . map toSet
  where
   toSet = S.fromList . T.unpack

threeGroups :: [a] -> [[a]]
threeGroups = chunksOf 3

part1 :: T.Text -> Int
part1 = sum . map priority . concatMap (S.toList . uncurry lineDiff . splitLine) . T.lines

rateGroup :: [T.Text] -> Int
rateGroup ls = priority c--  $ length $ filter (==c) $ concatMap T.unpack ls
  where [c] = S.toList (lineDiffs ls)
part2 :: T.Text -> Int
part2 = sum . map rateGroup . threeGroups . T.lines


main :: IO ()
main = print . part2  =<< input

