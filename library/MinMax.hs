---- Minimax and alpha-beta pruning
----
---- Minimax can trivially be generalized to work on any lattice (@gminimax@).
---- Then alpha-beta is actually an instance of minimax.
----
---- Contents:
---- 0. Basic definitions: players and games
---- 1. Direct implementations of minimax and alpha-beta
---- 2. Generalized minimax and instantiation to alpha-beta
---- 3. QuickCheck tests

--{-# LANGUAGE DeriveFunctor, StandaloneDeriving, ScopedTypeVariables, BlockArguments, FlexibleInstances, UndecidableInstances #-}
module MinMax where

--import Data.List.NonEmpty
--import Test.QuickCheck

---- * Part 0: Basic definitions

--newtype Fix f = Fix (f (Fix f))

--deriving instance Show (f (Fix f)) => Show (Fix f)

--cata :: (GameF a r -> r) -> Game a -> r
--cata f (Fix g) = f (fmap (cata f) g)

---- | One player wants to maximize the score,
---- the other wants to minimize the score.
--data Player = Mini | Maxi deriving Show

---- | At every step, either the game ended with a value,
---- or one of the players is to play.
--data GameF a r = Value a | Play Player (NonEmpty r)
-- deriving (Functor, Show)
--type Game a = Fix (GameF a)

---- * Part 1: Direct implementations

--minimax0 :: forall a. Ord a => Game a -> a
--minimax0 = cata minimaxF where
--  minimaxF :: GameF a a -> a
--  minimaxF (Value x) = x
--  minimaxF (Play p xs) = optimum p xs

--optimum :: Ord a => Player -> NonEmpty a -> a
--optimum Mini = minimum
--optimum Maxi = maximum

--alphabeta0 :: forall a. Ord a => Game a -> a
--alphabeta0 g = cata alphabetaF g (Bot, Top) where
--  alphabetaF :: GameF a (Interval a -> a) -> Interval a -> a
--  alphabetaF (Value x) = \_ -> x
--  alphabetaF (Play p xs) =
--    let go l r (alpha, beta) =
--          let vl = l (alpha, beta) in
--          case p of
--            Mini -> if NoBot vl <= alpha then vl else min vl (r (alpha, min (NoTop vl) beta))
--            Maxi -> if beta <= NoTop vl then vl else max vl (r (max (NoBot vl) alpha, beta)) in
--    foldr1 go xs

---- The function @go@ actually defines a lattice of functions @(Interval a -> a)@.

---- * Part 2: Generalized minimax

---- | Associative, commutative, idempotent,
---- and with absorption laws
----
---- > inf x (sup x y) = x
---- > sup x (inf x y) = x
--class Lattice l where
--  inf, sup :: l -> l -> l

--newtype Order a = Order { unOrder :: a }
--  deriving (Eq, Ord)

--instance Ord a => Lattice (Order a) where
--  inf = min
--  sup = max

---- | Generalized minimax
--gminimax :: Lattice l => (a -> l) -> Game a -> l
--gminimax leaf = cata minimaxF where
--  minimaxF (Value x) = leaf x
--  minimaxF (Play p xs) = foldr1 (lopti p) xs

--lopti :: Lattice l => Player -> l -> l -> l
--lopti Mini = inf
--lopti Maxi = sup

--minimax :: Ord a => Game a -> a
--minimax = unOrder . gminimax Order

---- | A pruning search for an @a@.
----
---- Constructing @Pruning f@ must ensure the following invariant:
----
---- > clamp i (f i) = clamp i (f (Bot, Top))
--newtype Pruning a = Pruning { unPruning :: Interval a -> a }

---- | Intervals.
----
---- @Interval a@ is @(Maybe a, Maybe a)@ but the @Nothing@ have
---- different interpretations so we use different types for clarity.
--type Interval a = (WithBot a, WithTop a)
--data WithBot a = Bot | NoBot a deriving (Eq, Ord)
--data WithTop a = NoTop a | Top deriving (Eq, Ord)

--clamp :: Ord a => Interval a -> a -> a
--clamp (l, r) = clampBot l . clampTop r

--clampBot :: Ord a => WithBot a -> a -> a
--clampBot Bot x = x
--clampBot (NoBot y) x = max y x

--clampTop :: Ord a => WithTop a -> a -> a
--clampTop Top x = x
--clampTop (NoTop y) x = min y x

---- Extra properties
----
---- > runPruning (inf l r) = min (runPruning l) (runPruning r)
---- > runPruning (sup l r) = max (runPruning l) (runPruning r)
--instance Ord a => Lattice (Pruning a) where
--  inf l r = Pruning \(alpha, beta) ->
--    let vl = unPruning l (alpha, beta) in
--    if NoBot vl <= alpha then vl else min vl (unPruning r (alpha, min (NoTop vl) beta))

--  sup l r = Pruning \(alpha, beta) ->
--    let vl = unPruning l (alpha, beta) in
--    if beta <= NoTop vl then vl else max vl (unPruning r (max (NoBot vl) alpha, beta))

--alphabeta :: Ord a => Game a -> a
--alphabeta = runPruning . gminimax constPruning

--constPruning :: a -> Pruning a
--constPruning = Pruning . const

--runPruning :: Pruning a -> a
--runPruning f = unPruning f (Bot, Top)

-----

--instance Arbitrary Player where
--  arbitrary = elements [Mini, Maxi]

--instance Arbitrary (Game Int) where
--  arbitrary = sized genGame

--genGame :: Int -> Gen (Game Int)
--genGame n = Fix <$> frequency
--  [ (1, Value <$> arbitrary)
--  , (4, Play <$> arbitrary <*> genGames n)
--  ]

--genGames :: Int -> Gen (NonEmpty (Game Int))
--genGames n = do
--  i <- choose (0, n)
--  genNonEmpty i (genGame (n `div` (i+1)))

--genNonEmpty :: Int -> Gen a -> Gen (NonEmpty a)
--genNonEmpty i g = (:|) <$> g <*> vectorOf i g

--size :: Game a -> Int
--size = cata sizeF where
--  sizeF (Value _) = 1
--  sizeF (Play _ xs) = sum xs

--main :: IO ()
--main = quickCheck \g -> collect (size g) $ minimax g === alphabeta (g :: Game Int)

