{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Day05 where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens.Regex.Text
import Control.Lens
import Data.List (transpose)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')

input :: IO T.Text
input = T.readFile "input/Day05.txt"

-- format 1:
testStacks :: T.Text
testStacks = T.unlines [
    "            [C]         [N] [R]    ",
    "[J] [T]     [H]         [P] [L]    ",
    "[F] [S] [T] [B]         [M] [D]    ",
    "[C] [L] [J] [Z] [S]     [L] [B]    ",
    "[N] [Q] [G] [J] [J]     [F] [F] [R]",
    "[D] [V] [B] [L] [B] [Q] [D] [M] [T]",
    "[B] [Z] [Z] [T] [V] [S] [V] [S] [D]",
    "[W] [P] [P] [D] [G] [P] [B] [P] [V]"
 ]

parseLine1 :: T.Text -> [Maybe Char]
parseLine1 = map (parseSingle . T.unpack) . T.chunksOf 4 
  where
    parseSingle ('[': c: ']': _) = Just c
    parseSingle _ = Nothing

parseStacks :: T.Text -> M.Map Int [Char]
parseStacks = M.fromList . zip [1..] . map (map fromJust . dropWhile isNothing) . transpose . map parseLine1 . T.lines

data Move = Move {source :: Int, dest :: Int, amount:: Int} deriving (Show, Eq)
parseMove :: T.Text -> [Move]
parseMove t = map parseMove' $ t ^.. [regex|move ([0-9]+) from ([0-9]+) to ([0-9]+)|] . groups . to toInt
  where
    parseMove' [amount, source, dest] = Move {..}
    parseMove' _ = error "impossible"
    toInt :: [T.Text] -> [Int]
    toInt = map (read . T.unpack)

applyAmount :: M.Map Int [Char] -> Move -> M.Map Int [Char]
applyAmount m Move{..} = M.adjust (moved <>) dest $ M.insert source source' m
  where (moved, source') = splitAt amount (m M.! source)

main :: IO ()
main = do
   inp <- input
   let (l,r) = T.breakOn "\n\n" inp
       stacks = parseStacks (T.init l)
       acts = parseMove r
       firstOrSpace (x:_) = x
       firstOrSpace [] = '_'
   putStrLn $ map firstOrSpace $ M.elems $ foldl' applyAmount stacks acts
  
