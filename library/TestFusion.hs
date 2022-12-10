{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dsuppress-uniques -fforce-recomp #-}

module TestFusion (tester) where
import GHC.Base (build)
import Data.List (foldl')
import GHC.Exts (oneShot)

data SP a = SP { spIdx :: !Int, spVal :: a }
enumerate :: [a] -> [(Int, a)]
enumerate ls = build $ oneShot $ \c n -> spVal $ foldr (oneShot $ \xs (SP i x) -> SP (i+1) (c (i,xs) x)) (SP 0 n) ls


tester :: Int
tester = sum $ map (uncurry (*)) $ enumerate [1..10]
