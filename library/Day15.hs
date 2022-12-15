module Day15 where


import qualified Data.IntervalSet as S
import Text.Peggy
import qualified Data.Interval as M
import qualified Data.Vector.Unboxed as V

-- Didn't feel like writing smart code, so this one takes ~30 seconds

type Point = (Int, Int)
[peggy|
parseLine :: [(Point, Point)]
  = ("Sensor at " parsePos ": closest beacon is at " parsePos)*
parsePos :: Point = "x="number", y="number
number :: Int = [0-9]+ {read $1}/ "-"number {negate $1}
|]


toRombus :: (Point, Point) -> (Point, Int)
toRombus ((x, y), (x', y')) =((x,y), diag)
  where
     colDiff = abs (y - y')
     rowDiff = abs (x - x')

     diag = colDiff + rowDiff
     -- straight = round (fromIntegral diag / sqrt 2)


atRow :: (Ord r, Num r) => r -> ((r, r), r) -> M.Interval r
atRow yTarget ((x,y),j)  = M.Finite spanL M.<=..<= M.Finite spanR
  where
      delta = abs (y - yTarget)
      left = max 0 (j - delta)

      spanL = x - left
      spanR 
        | left > 0 = x + left
        | otherwise = spanL - 1


toIntervalsAt :: Int ->V.Vector (Point, Point) -> S.IntervalSet Int
toIntervalsAt y ps =  S.fromList (fmap (atRow y . toRombus) (V.toList ps))

leftOver :: Int -> M.Interval Int -> V.Vector (Point,Point) -> S.IntervalSet Int
leftOver y range points = S.difference (S.singleton range) (toIntervalsAt y points)

toPoint :: Ord r => S.IntervalSet r -> [M.Interval r]
toPoint s = filter (not . M.null) $ S.toList  s

firstLeftover :: Int -> Int -> V.Vector (Point, Point) -> [[(M.Interval Int, Int)]]
firstLeftover l r points = filter (not . null) [ (,y) <$> toPoint (leftOver y range points) | y <- [l..r]]
  where
    range = M.Finite l M.<=..<= M.Finite r


inputs :: IO (V.Vector (Point, Point))
inputs = V.fromList . either (error . show) id <$> parseFile parseLine "input/Day15.txt"
main :: IO ()
main = do
   inp <- inputs
   print $ setSize $ toIntervalsAt 2000000 inp
   print $ firstLeftover 0 4000000 inp

setSize :: S.IntervalSet Int -> Int
setSize = sum . map M.width . S.toList
