{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day02 where

input :: IO [String]
input = lines <$> readFile "input/Day02.txt"

data Act = Rock | Paper | Scissors deriving (Show, Eq, Ord, Enum, Bounded)

offset :: Int -> Act -> Act
offset n a = toEnum $ (fromEnum a + n) `mod` 3

toLose :: Act -> Act
toLose = offset (-1)

toWin :: Act -> Act
toWin = offset 1

toDraw :: Act -> Act
toDraw = offset 0

scoreSelf :: Act -> Int
scoreSelf Rock = 1
scoreSelf Paper = 2
scoreSelf Scissors = 3

data Outcome = Draw | Win | Loss deriving (Show, Eq, Ord)

outcome :: Act -> Act -> Outcome
outcome a b
  | a == b = Draw
outcome Rock Scissors = Win
outcome Scissors Paper = Win
outcome Paper Rock = Win
outcome _ _ = Loss

scoreOutcome :: Outcome -> Int
scoreOutcome Draw = 3
scoreOutcome Win = 6
scoreOutcome Loss = 0

totalScore :: Act -> Act -> Int
totalScore a b = scoreSelf b + scoreOutcome (outcome b a)

parseLine :: [Char] -> (Act, Act)
parseLine [a,_,c] = (parseYou a, parseMe c)
  where
    parseMe 'X' = Rock
    parseMe 'Y' = Paper
    parseMe 'Z' = Scissors
    parseYou 'A' = Rock
    parseYou 'B' = Paper
    parseYou 'C' = Scissors
stratMap :: [String] -> [(Act, Act)]
stratMap = map parseLine


actToGoal :: Act -> Act -> Act
actToGoal a Rock = toLose a
actToGoal a Paper = toDraw a
actToGoal a Scissors = toWin a

totalScore2 :: Act -> Act -> Int
totalScore2 a b = totalScore a (actToGoal a b)

-- part1 :: [[Char]] -> Int
part1 :: [[Char]] -> Int
part1 = sum . map (uncurry totalScore . parseLine)
part2 :: [[Char]] -> Int
part2 = sum . map (uncurry totalScore2 . parseLine)
main :: IO ()
main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp

