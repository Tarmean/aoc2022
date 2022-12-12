{-# LANGUAGE RecordWildCards #-}
module Day12 where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Heap
import qualified Data.Set as S
import Control.Monad (guard, unless)
import Data.Monoid (Sum(..))
import Control.Monad.Writer (MonadWriter(tell))
import Control.Applicative (asum)

data BFSConfig a = BFSConfig { neighbours :: a -> [a]
                            , goal :: a
                            , source :: [a]
                            }

type M a = HeapT (Sum Int) (State (S.Set a))

bfsSearch :: (Ord a) => BFSConfig a -> [Sum Int]
bfsSearch BFSConfig{..} = runM (pick source >>= go)
  where
    go cur = unless (cur == goal) $ do
        next <- pick (neighbours cur)
        tell (Sum 1)
        go next
    runM = fmap snd . flip evalState mempty . searchT

unique :: Ord a => a -> M a a
unique x = do
  unseen <- gets (S.notMember x)
  guard unseen
  modify (S.insert x)
  pure x
{-# INLINE unique #-}
pick :: Ord a => [a] -> M a a
pick = asum . map unique
{-# INLINE pick #-}

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

part1 :: [String] -> Sum Int
part1 ls = head (bfsSearch BFSConfig{..})
  where
    grid0 = toGrid ls
    grid = normStartEnd grid0
    neighbours = getNeighbours grid
    goal = getEnd grid0
    -- source = [getStart grid0]
    source = M.keys $ M.filter (== 'a') grid
inp :: [String]
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

