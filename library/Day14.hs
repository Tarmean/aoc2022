module Day14 where
import qualified Data.Map as M
import Text.Peggy

[peggy|
parseInp :: [Pos] = (number","number,"->")
number :: Int = [0-9]+ {read $1}
|]

fullLines :: [Pos] -> [Pos]
fullLines (x:y:xs) = fromTo x y <> fullLines (y:xs)
  where
    fromTo (a,b) (c,d) = [(a',b') | a' <- range a c, b' <- range b d]
    range l r = [min l r .. max l r]
fullLines _ = []

input :: IO Grid
input = do
   file <- readFile "input/Day14.txt"
   let lin = (either (error.show) id . parseString parseInp "inp") <$> lines file 
   pure $ toGrid $ concatMap fullLines (lin :: [[Pos]])

type Pos = (Int, Int)
data Grid = Grid { content :: M.Map Pos Item, bounds :: (Pos, Pos) } deriving (Show, Eq, Ord)

toGrid :: [Pos] -> Grid
toGrid ps = Grid (M.fromList $ (,Stone) <$>  ps) ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs = map fst ps
    ys = map snd ps

printGrid :: Grid -> IO ()
printGrid g = mapM_ putStrLn $ do
    y <- [y1..y2]
    pure $ do
        x <- [x1..x2]
        pure $ case M.lookup (x,y) (content g) of
            Just Stone -> '#'
            Just Sand -> 'o'
            _ -> '.'
    where ((x1,y1),(x2,y2)) = bounds g

data Item = Stone | Sand | Air deriving (Show, Eq, Ord)

inBounds :: Grid -> Pos -> Bool
inBounds _ _ = True
-- inBounds g (x, y) = y1 >= y
--   where ((x0, y0), (x1, y1)) = bounds g

windowBelow :: Grid -> Pos -> [Item]
windowBelow g (x, y)
  | y == y1+1 = [Stone,Stone,Stone]
  | otherwise = map (\x' -> M.findWithDefault Air x' (content g)) [(x-1, y+1), (x, y+1), (x+1, y+1)]
  where
    ((_, _), (_, y1)) = bounds g

nextPos :: [Item] -> Pos -> Maybe Pos
nextPos [_, Air, _] (x, y) = Just (x, y+1)
nextPos [Air, _, _] (x, y) = Just (x-1, y+1)
nextPos [_, _, Air] (x, y) = Just (x+1, y+1)
nextPos _ _ = Nothing

insertPiece :: Grid -> Pos -> Grid
insertPiece g p = g { content = M.insert p Sand (content g) }
simulateSand :: Grid -> Maybe Grid
simulateSand g
  | (500, 0) `M.member`content g = Nothing
  | otherwise = go (500,0)
  where
    go p
      | not (inBounds g p) = Nothing
      | otherwise = case nextPos (windowBelow g p) p of
         Nothing -> Just (insertPiece g p)
         Just p' -> go p'
main :: IO ()
main = do
    inp <- input
    let
      go g = do
        -- putStrLn (replicate 10 '-')
        -- printGrid g
        case simulateSand g of
          Nothing -> pure g
          Just g' -> go g'
    out <- go inp
    print $ length $ [() | (Sand) <- M.elems (content out)]
