{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Day04 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens
import Control.Lens.Regex.Text

input :: IO T.Text
input = T.readFile "input/Day04.txt"

-- format: 8-82,3-96
type Range = (Int, Int)
parse :: T.Text -> [(Range, Range)]
parse t = t ^.. [regex|(\d+)-(\d+),(\d+)-(\d+)|] . groups . to (map toInt) . to (\[a,b,c,d] -> ((a,b),(c,d)))
  where
    toInt = read . T.unpack

containedBy :: Range -> Range -> Bool
containedBy (a,b) (c,d) = a >= c && b <= d

overlappedBy :: Range -> Range -> Bool
overlappedBy (a,b) (c,d) = a <= c && b >= c || a <= d && b >= d
overlappedBy2 :: Range -> Range -> Bool
overlappedBy2 (a,b) (c,d) = not (b < c || d < a)
overlappedBy3 :: Range -> Range -> Bool
overlappedBy3 (a,b) (c,d) = a <= d && c <= b


part1 :: T.Text -> Int
part1 = length . filter (\(x, y) -> x `containedBy` y || y `containedBy` x) . parse

main :: IO ()
main = print . part1 =<< input
