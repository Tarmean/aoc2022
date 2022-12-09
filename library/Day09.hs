{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day09 where


import Linear.V2
import qualified Data.Set as S
import qualified Data.Map.Lazy as M
import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Control.Lens
import Data.List (maximumBy)
import Control.Monad.Zip (MonadZip(mzipWith))



type Point = V2 Int

newtype Snake = S { pieces :: M.Map Int Point }


moveSnake :: Snake -> Point -> Snake
moveSnake (S m) d = S m'
  where
    m' = M.mapWithKey step1 m
    step1 0 p = p + d
    step1 i p 
      | not (outOfCycle p newP) = p
      | otherwise = mzipWith move p newP
      where
        newP = m' M.! (i - 1)
        move a b = signum (b-a) + a

outOfCycle :: Point -> Point -> Bool
outOfCycle a b = 1 < taxi a b
taxi :: Point -> Point -> Int
taxi (V2 x1 y1) (V2 x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

visited :: Snake -> [Point] -> [Snake]
visited = scanl moveSnake

data Command = U | D | L | R deriving Show
parseInput :: [String] -> [Command]
parseInput = concatMap (parseLine . words)
  where
    parseLine [c, n] = replicate (read n) (parseCommand c)
    parseCommand "U" = U
    parseCommand "D" = D
    parseCommand "L" = L
    parseCommand "R" = R

commandToDir :: Command -> Point
commandToDir R = V2 0 1
commandToDir L = V2 0 (-1)
commandToDir U = V2 (-1) 0
commandToDir D = V2 1 0

visitedPoints :: Snake -> [String] -> [Snake]
visitedPoints s = visited s . map commandToDir . parseInput

printSnake :: Snake -> IO ()
printSnake m = do
    putStrLn "-------"
    let (V2 x1 y1, V2 x2 y2) = bounds m
    mapM_ putStrLn $ do
       x <- [x1..x2]
       pure $ do
         y <- [y1..y2]
         let tiles = [k | (k,v) <- M.toList (pieces m), v == V2 x y]
         if not (null tiles) then show (minimum tiles)
         else if V2 x y == 0 then "s"
         else "."

bounds :: Snake -> (V2 Int, V2 Int)
bounds (S m) = (V2 x1 y1, V2 x2 y2)
  where
    x1 = minimum1Of (each ._x) $ M.elems m
    x2 = maximum1Of (each ._x) $ M.elems m
    y1 = minimum1Of (each ._y) $ M.elems m
    y2 = maximum1Of (each ._y) $ M.elems m

main :: IO ()
main = do
    inp <- readFile "input/Day09.txt"
    let len = 9
    let startPos = S (M.fromList [(i, 0) | i <- [0..len]])
    -- mapM_ printSnake $ (visitedPoints startPos) inp
    print $ S.size $ S.fromList $ map ((M.! len) . pieces) $ visitedPoints startPos $ lines inp
