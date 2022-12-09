{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Day08 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.List (transpose)
import qualified Data.Set as S
import Test.QuickCheck
import qualified Data.Vector.Unboxed as V
import GHC.OldList (sortOn)
import Data.Ord (Down(..))

input :: IO T.Text
input = T.readFile "input/Day08.txt"

-- 2d water flow

solve1 :: [[Char]] -> S.Set (Int, Int)
solve1 ls = foldMap (toPosition id) $ belowOrientations (map visible) matrix
  where matrix = map (map digitToInt) ls

visible :: [Int] -> [Bool]
visible ls = zipWith (>) ls ((-1) : scanl1 max ls)

belowOrientations :: ([[a]] -> [[b]]) -> [[a]] -> [[[b]]]
belowOrientations f ls =  do
  SomeTrans l r <- orientations -- this line typechecks
  pure $ r (f (l ls))
data SomeTrans = SomeTrans (forall x. ([[x]] -> [[x]])) (forall y. ([[y]] -> [[y]]))
orientations :: [SomeTrans]
orientations = [someTrans id, someTrans transpose, someTrans (fmap reverse), SomeTrans (fmap reverse . transpose) (transpose . fmap reverse)]
  where
    someTrans :: (forall x. [[x]] -> [[x]]) -> SomeTrans
    someTrans a = SomeTrans a a

belowOrientations' :: ([[a]] -> [[b]]) -> [[a]] -> [[[b]]]
belowOrientations' f ls =  do
  -- (l, r)::SomeTrans' <- orientations' -- this line does not typecheck
  orientations' >>= \(l,r) -> -- this line typechecks
   pure $ r (f (l ls))
type SomeTrans' = (forall x. ([[x]] -> [[x]]), forall y. ([[y]] -> [[y]]))
orientations' :: [SomeTrans']
orientations' = [someTrans id, someTrans transpose, someTrans (fmap reverse), (fmap reverse . transpose,transpose . fmap reverse)]
  where
    someTrans :: (forall x. [[x]] -> [[x]]) -> SomeTrans'
    someTrans a = (a, a)


toPosition :: (a -> Bool) -> [[a]] -> S.Set (Int, Int)
toPosition p ls = S.fromList [ (x, y) | (y, row) <- zip [0..] ls, (x, v) <- zip [0..] row, p v]


-- test :: IO ()
-- test = quickCheck $ \(ls::[[Int]]) -> all (not . null) ls ==> and $ do
--   (l, r) <- undefined -- orientations'
--   pure $ ls == r ( l ls)




type Pos = (Int, Int)
(!) :: V.Unbox a => V.Vector a -> Pos -> a
v ! (y, x) = v V.! (y * width + x)
width :: Int
width = 99
scoreFor :: Pos -> V.Vector Int -> Int
scoreFor p m = product [go0 (1,0) p, go0 (-1,0) p, go0 (0,1) p, go0 (0,-1) p]
  where
    (.+) (a,b) (c,d) = (a+c, b+d)
    inBounds (x,y) = x >= 0 && x < width && y >= 0 && y < width
    start = m ! p
    go0 dir pos = go 0 dir (pos .+ dir)
    go acc dir pos
      | not (inBounds pos) = acc
      | m ! pos >= start = acc+1
      | otherwise =  go (acc+1) dir (pos .+ dir)

posScores :: V.Vector Int -> [((Int, Int), Int)]
posScores m = do
  y <- [0..width-1]
  x <- [0..width-1]
  let p = (x,y)
  pure (p, scoreFor p m)

main :: IO ()
main = do
  inp <- input
  print $  S.size . solve1 $ map T.unpack $ T.lines inp
  let vecs = V.fromList . map digitToInt $ concatMap T.unpack $ T.lines inp
  print $ take 10 $ sortOn  (Down . snd) $ posScores vecs

