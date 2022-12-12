{-# LANGUAGE RecordWildCards #-}
module Day12 where

import qualified Data.Map as M
import Control.Monad.State
import Debug.Trace (trace)
import qualified Data.Set as S
import Data.Maybe (catMaybes)

data BFSConfig a = BFSConfig { neighbours :: a -> [a]
                            , goal :: a
                            , source :: a
                            }

type M a = State (M.Map a Int)


bfsSearch :: (Show a, Ord a) => BFSConfig a -> M.Map a Int
bfsSearch BFSConfig{..} = go mempty (S.singleton source) 0
  where
    go vis ls _ | S.null ls = vis
    go vis curs dist = go vis' next' dist'
      where
        next = S.fromList $ concatMap neighbours curs
        next' = S.filter (not . (`M.member` vis')) next
        dist' = dist + 1
        vis' = M.union vis $ M.fromList $ zip (S.toList curs) $ repeat dist

-- getBFSResult :: (Ord a) => BFSConfig a -> Int
type Pos = (Int, Int)
toGrid :: [String] -> M.Map Pos Char
toGrid ls = M.fromList [( (y, x), c) | (y, row) <- zip [0..] ls, (x, c) <- zip [0..] row]

getNeighbours :: M.Map Pos Char -> Pos -> [Pos]
getNeighbours grid (x, y) = filter isPassable [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
  where
    isPassable n 
      | Just o <- M.lookup n grid = o <= succ (grid M.! (x,y))
      | otherwise = False
getStart :: M.Map Pos Char -> Pos
getStart grid = head $ M.keys $ M.filter (== 'S') grid

getEnd :: M.Map Pos Char -> Pos
getEnd grid = head $ M.keys $ M.filter (== 'E') grid

normStartEnd :: M.Map Pos Char -> M.Map Pos Char
normStartEnd grid = M.insert (getEnd grid) 'z' $ M.insert (getStart grid) 'a' grid

-- part1 :: [String] -> Int
part1 ls = minimum $ catMaybes [ bfsSearch BFSConfig{..} M.!? goal | source <- posSources ]
  where
    grid0 = toGrid ls
    grid = normStartEnd grid0
    neighbours = getNeighbours grid
    goal = getEnd grid0
    -- source = getStart grid0
    posSources = M.keys $ M.filter (== 'a') grid
inp= [
    "abaacccccccccccccaaaaaaaccccccccccccccccccccccccccccccccccaaaaaa",
    "abaaccccccccccccccaaaaaaaaaaccccccccccccccccccccccccccccccccaaaa",
    "abaaaaacccccccccaaaaaaaaaaaaccccccccccccccccccccccccccccccccaaaa",
    "abaaaaaccccccccaaaaaaaaaaaaaacccccccccccccccccdcccccccccccccaaaa",
    "abaaaccccccccccaaaaaaaaccacacccccccccccccccccdddcccccccccccaaaaa",
    "abaaacccccccccaaaaaaaaaaccaaccccccccccccciiiiddddcccccccccccaccc",
    "abcaaaccccccccaaaaaaaaaaaaaaccccccccccciiiiiijddddcccccccccccccc",
    "abccaaccccccccaccaaaaaaaaaaaacccccccccciiiiiijjddddccccaaccccccc",
    "abccccccccccccccaaacaaaaaaaaaaccccccciiiiippijjjddddccaaaccccccc",
    "abccccccccccccccaacccccaaaaaaacccccciiiippppppjjjdddddaaaaaacccc",
    "abccccccccccccccccccccaaaaaaccccccckiiippppppqqjjjdddeeeaaaacccc",
    "abccccccccccccccccccccaaaaaaccccckkkiippppuupqqjjjjdeeeeeaaccccc",
    "abccccccccccccccccccccccccaaccckkkkkkipppuuuuqqqjjjjjeeeeeaccccc",
    "abccccccccccccccccccccccccccckkkkkkoppppuuuuuvqqqjjjjjkeeeeccccc",
    "abcccccccccccccccccccccccccckkkkooooppppuuxuvvqqqqqqjkkkeeeecccc",
    "abccaaccaccccccccccccccccccckkkoooooopuuuuxyvvvqqqqqqkkkkeeecccc",
    "abccaaaaacccccaaccccccccccckkkoooouuuuuuuxxyyvvvvqqqqqkkkkeecccc",
    "abcaaaaacccccaaaacccccccccckkkooouuuuxxxuxxyyvvvvvvvqqqkkkeeeccc",
    "abcaaaaaaaaaaaaacccccccccccjjjooottuxxxxxxxyyyyyvvvvrrrkkkeecccc",
    "abcccaaaacaaaaaaaaacaaccccccjjoootttxxxxxxxyyyyyyvvvrrkkkfffcccc",
    "SbccaacccccaaaaaaaaaaaccccccjjjooottxxxxEzzzyyyyvvvrrrkkkfffcccc",
    "abcccccccccaaaaaaaaaaaccccccjjjooootttxxxyyyyyvvvvrrrkkkfffccccc",
    "abcaacccccaaaaaaaaaaaccccccccjjjooottttxxyyyyywwvrrrrkkkfffccccc",
    "abaaacccccaaaaaaaaaaaaaacccccjjjjonnttxxyyyyyywwwrrlllkfffcccccc",
    "abaaaaaaaaaaacaaaaaaaaaaccccccjjjnnnttxxyywwyyywwrrlllffffcccccc",
    "abaaaaaaaaaaaaaaaaaaaaaaccccccjjjnntttxxwwwwwywwwrrlllfffccccccc",
    "abaaccaaaaaaaaaaaaaaacccccccccjjjnntttxwwwsswwwwwrrlllfffccccccc",
    "abaacccaaaaaaaacccaaacccccccccjjinnttttwwsssswwwsrrlllgffacccccc",
    "abccccaaaaaaccccccaaaccccccccciiinnntttsssssssssssrlllggaacccccc",
    "abccccaaaaaaaccccccccccaaccccciiinnntttsssmmssssssrlllggaacccccc",
    "abccccaacaaaacccccccaacaaaccccciinnnnnnmmmmmmmsssslllgggaaaacccc",
    "abccccccccaaacccccccaaaaacccccciiinnnnnmmmmmmmmmmllllgggaaaacccc",
    "abaaaccccccccccccccccaaaaaacccciiiinnnmmmhhhmmmmmlllgggaaaaccccc",
    "abaaaaacccccccccccaaaaaaaaaccccciiiiiiihhhhhhhhmmlgggggaaacccccc",
    "abaaaaaccccaaccccaaaaaaacaacccccciiiiihhhhhhhhhhggggggcaaacccccc",
    "abaaaaccccaaaccccaaaacaaaaacccccccciiihhaaaaahhhhggggccccccccccc",
    "abaaaaaaacaaacccccaaaaaaaaaccccccccccccccaaaacccccccccccccccccaa",
    "abaacaaaaaaaaaaaccaaaaaaaaccccccccccccccccaaaccccccccccccccccaaa",
    "abcccccaaaaaaaaacccaaaaaaaccccccccccccccccaacccccccccccccccccaaa",
    "abccccccaaaaaaaaaaaaaaaaacccccccccccccccccaaacccccccccccccaaaaaa",
    "abcccccaaaaaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccaaaaaa"
    ]

