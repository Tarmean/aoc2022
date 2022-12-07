{-# LANGUAGE OverloadedStrings #-}
module Day01 where

-- Generic imports
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Data.List.Split (splitOn, splitWhen)
import Data.List (sort, sortOn)
import Data.Ord (Down(Down))


input :: IO T.Text
input = T.readFile "input/Day01.txt"

-- | Part 1
-- split input into groups of lines, seperated by empty line

groups :: T.Text -> [[T.Text]]
groups = splitWhen (=="")  . T.lines

sumGroup :: [T.Text] -> Int
sumGroup = sum . map (read . T.unpack . T.filter (/=' '))

part1 :: T.Text -> Int
part1 = maximum . map sumGroup . groups

-- | Part 2

part2 :: T.Text -> Int
part2 = sum . take 3 . sortOn Down . map sumGroup . groups

main :: IO ()
main = do
   inp <- input
   print $ part1 inp
   print $ part2 inp
