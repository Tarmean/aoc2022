{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day13 where
import Text.Peggy
import Data.List (sort, elemIndex)

data RoseTree a = Leaf a | RoseTree [RoseTree a]
  deriving (Show, Eq)
sing :: a -> RoseTree a
sing a = RoseTree [Leaf a]
instance Ord a => Ord (RoseTree a) where
  compare (Leaf x) (Leaf y) = compare x y
  compare (Leaf x) a = compare (sing x) a
  compare a (Leaf x) = compare a (sing x)
  compare (RoseTree x) (RoseTree y) = compare x y

[peggy|
roseTree :: RoseTree Int = int {Leaf $1} / "[" (roseTree, ",") "]" { RoseTree $1 }
rosePairs :: [(RoseTree Int, RoseTree Int)] = (roseTree roseTree)*
int :: Int = [0-9]+ { read $1 }
|]

input :: IO  [(RoseTree Int, RoseTree Int)]
input = either (error . show) id <$> parseFile rosePairs "input/Day13.txt"

main :: IO ()
main = do
  pairs <- input
  print $ sum $ map (succ . fst) $ filter (uncurry (<=) . snd)$  zip [0::Int ..] pairs
  let sorted = sort $ sing 2:sing 6:concat [[a,b] | (a,b) <- pairs]
  print $ elemIndex (sing 2) sorted
  print $ elemIndex (sing 6) sorted
