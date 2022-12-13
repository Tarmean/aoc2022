{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day13 where
import Text.Peggy
import Data.List (sort, elemIndex)

data RoseTree a = Leaf a | RoseTree [RoseTree a] deriving (Show, Eq)
[peggy|
roseTree :: RoseTree Int = int {Leaf $1} / "[" (roseTree, ",") "]" { RoseTree $1 }
rosePairs :: [(RoseTree Int, RoseTree Int)] = (roseTree roseTree)*
int :: Int = [0-9]+ { read $1 }
|]

input :: IO  [(RoseTree Int, RoseTree Int)]
input = either (error . show) id <$> parseFile rosePairs "input/Day13.txt"

compareTree :: Ord a => RoseTree a -> RoseTree a -> Ordering
compareTree (Leaf a) (Leaf b) = compare a b
compareTree (Leaf l) (RoseTree r) = compareTree (RoseTree [Leaf l]) (RoseTree r)
compareTree (RoseTree l) (Leaf r) = compareTree (RoseTree l) (RoseTree [Leaf r])
compareTree (RoseTree (x:xs)) (RoseTree (y:ys)) = compareTree x y <> compareTree (RoseTree xs) (RoseTree ys)
compareTree (RoseTree []) (RoseTree []) = EQ
compareTree (RoseTree []) _ = LT
compareTree _ (RoseTree []) = GT
instance Ord a => Ord (RoseTree a) where
  compare = compareTree

main :: IO ()
main = do
  pairs <- input
  print $ sum $ map (succ . fst) $ filter (uncurry (<=) . snd)$  zip [0::Int ..] pairs
  let t1 = RoseTree [Leaf 2] 
  let t2 = RoseTree [Leaf 6]
  let sorted = sort $ t1:t2:concat [ [a,b] | (a,b) <- pairs]
  print $ elemIndex t1 sorted
  print $ elemIndex t2 sorted
