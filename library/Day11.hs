{-# Language QuasiQuotes #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Day11 where

import Text.Peggy
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(Down))


type Item = Integer
type MonkeyId = Integer

[peggy|
monkeys :: [Monkey]
    = monkeys:monkey*  { monkeys }
monkey :: Monkey
    = "Monkey" n:integer ":"
        "Starting items:" items:itemList
        "Operation:" op:operation
        "Test:" test:test
        branches:branches { Monkey n items op test branches 0 }
operation :: (Integer -> Integer)
    = "new" "=" l:operationArg op:("+" {(+)}/ "-" {(-)}/ "*" {(*)}/ "/"{div}) r:operationArg {
     \x -> op (fromMaybe x l) (fromMaybe x r)
        }
operationArg :: Maybe Integer 
  = arg:integer { Just arg }
  / "old" { Nothing }
branches :: (Integer, Integer)
  = "If true: throw to monkey" n1:integer 
    "If false: throw to monkey" n2:integer { (n1, n2) }
test :: (Integer)
  = "divisible by" n:integer { n }
itemList :: [Integer]
  = items:(integer, ",") { items }
integer :: Integer
  = [0-9]+ { read $1 }
|]

data Monkey = Monkey { mid :: MonkeyId, items :: [Item], stepFun :: Item -> Item, cond :: Integer, choices :: (MonkeyId, MonkeyId), count :: Integer }


toOutputs :: Monkey -> M.Map MonkeyId [Item]
toOutputs m = M.fromListWith (<>) $ do
    i <- items m
    let o = stepFun m i
    if  mod o (cond m) == 0
        then [(fst $ choices m, [o])]
        else [(snd $ choices m, [o])]

type M = State (M.Map MonkeyId Monkey)

stepMonkey :: Integer -> Integer -> M ()
stepMonkey crt i = do
    m <- gets (M.! i)
    modify $ M.adjust (\m -> m { items = [], count = count m + fromIntegral (length (items m)) }) i
    forM_ (M.toList (toOutputs m)) $ \(k,vs) -> do
        modify $ M.adjust  (\m -> m { items = sort (items m <> map (`mod` crt) vs) }) k

cycleMonkeys :: M.Map MonkeyId Monkey -> M.Map MonkeyId Monkey
cycleMonkeys m = flip execState m $ replicateM_ 10000 (mapM_ (stepMonkey crt) (M.keys m))
  where
    crt = product $ map cond $ M.elems m

main :: IO ()
main = do
   inp <- parseFile  monkeys "input/Day11.txt"
   case inp of
      Left err -> print err
      Right ms -> do
          let m = M.fromList $ zip (mid <$> ms) ms
          print $ product $ take 2 $ sortOn  Down  $ [ count mon | mon <- M.elems $ cycleMonkeys m]
