{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Day16 where
import Control.Monad.State
import Day12(bfsSearch, BFSConfig(..))


import Text.Peggy
import qualified Data.Map as M
import Data.Monoid (Sum(getSum))
import qualified Data.Set as S
import Control.Monad (guard, when, unless)
import Data.List (sortOn)
import Control.Applicative
import Data.Ord (Down(..), comparing)
import Debug.Trace (traceM, traceShowId)
import GHC.Stack (HasCallStack)
import Data.Foldable (maximumBy)
-- Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
-- Valve BB has flow rate=13; tunnels lead to valves CC, AA

data Room = Room { sector :: String, flow :: Int, exits :: [String] } deriving (Show)

[peggy|
locationP :: [Char] = [A-Z]+
flowP :: Int = f:[0-9]+ { read f }
lineP :: Room = "Valve" locationP "has flow rate=" flowP ";" ("tunnel leads to valve"/"tunnels lead to valves") (locationP,",") { Room $1 $2 $4 }
linesP :: [Room] = lineP*
|]


data SearchState = SearchState { precDists :: M.Map String (M.Map String Int), rooms :: M.Map String Room, score :: Integer, time :: Int, turned :: S.Set String, here :: String }
  deriving Show

type M = StateT SearchState []

distance :: M.Map String Room -> String -> String -> Int
distance rooms source goal = getSum $ head $ bfsSearch BFSConfig { neighbours = \x -> maybe [] exits (rooms M.!? x), goal = goal, source = [source] }

precompDistances :: M.Map String Room -> M.Map String (M.Map String Int)
precompDistances rooms = M.fromList $ do
  k <- M.keys rooms
  pure (k, M.fromList $ do
    k' <- M.keys rooms
    pure (k', distance rooms k k'))
    

goToRoom :: String -> M ()
goToRoom lab = do
   loc <- gets here
   dist <- gets ((M.! lab) . (M.! loc) . precDists)
   modify $ \s -> s {  time = time s + dist, here = lab }
turnValve :: Int -> M ()
turnValve duration = do
   loc <- gets here
   turn <- gets turned
   when (S.member loc turn) (error "Illegal turn")
   modify $ \s -> 
     let amount = flow (rooms s !!! loc)
         time' = time s + 1
         remaining = duration - time'
         score' = score s + fromIntegral (amount * remaining)
         turned' = S.insert loc (turned s)
     in s { score = score', time = time', turned = turned' }

(!!!) :: (Ord k, HasCallStack, Show k) => M.Map k v -> k -> v
m !!! k = case M.lookup k m of
  Nothing -> error $ "Key not found: " ++ show k
  Just v -> v
options :: M [String]
options = do
   r <- gets rooms
   vis <- gets turned
   pure [sector r | r <- M.elems r, sector r `S.notMember` vis, flow r > 0]

pick :: (Monad m, Alternative m) => [a] -> m a
pick ls = asum (map pure ls)
solve :: Int -> M ()
solve duration = pure () <|> do
    t <- gets time
    when (t < duration) $ do
        opts <- options
        unless (null opts) $ do
            lab <- pick opts
            goToRoom lab
            turnValve duration
            solve duration
runM' :: SearchState -> M r -> SearchState
runM' rs = maximumBy (comparing score) . flip execStateT rs
makeState :: [Room] -> SearchState
makeState rs = SearchState (precompDistances m) m  0 0 S.empty "AA"
 where m = M.fromList [(sector r, r) | r <- rs]


main :: IO ()
main = do
  rs <- either (error . show) id <$> parseFile linesP "input/Day16.txt"
  print $ score $ runM' (makeState rs) (solve 30)
  let
    st = runM' (makeState rs) $ do
        solve 26
        modify $ \s -> s { time = 0, here = "AA" }
        solve 26
  print $ score st
