module Day18 where

-- Idea Part 1: Generate all faces, check for faces which occur once
-- Idea Part 2: For each face, generate adjacent faces. Throw them into a union
--              find data structure and grab the largest equivalence class

import Control.Monad.Trans.UnionFind
import qualified Data.Map as M

import Linear.V3
import qualified Data.Set as S
import Control.Monad (forM_, forM)
import Conduit (Identity(runIdentity))
import Data.List.Split (splitOn)
import Data.Foldable (maximumBy)
import Data.Ord (comparing, Down (..))

type Pos = V3 Int
type Dir = V3 Int

type Face = (Pos, Dir)


zP :: Pos
zP = V3 0 0 1
xP :: Pos
xP = V3 1 0 0
yP :: Pos
yP = V3 0 1 0

facesForCube :: Pos -> [Face]
facesForCube = map norm . unnormFacesForCube
unnormFacesForCube :: Pos -> [Face]
unnormFacesForCube p = [(p, xP), (p, yP), (p, zP)]
              <> [(p, -xP), (p, -yP), (p, -zP)]


airFaces :: [Pos] -> [Face]
airFaces ps = M.keys . M.filter (== 1) . M.fromListWith (+) . map (,1::Int) $ concatMap facesForCube ps

cubeConnections :: S.Set Pos -> Pos -> Dir -> [Face]
cubeConnections cubes pos dir
    | S.member (pos + dir) cubes = []
    | otherwise = [single dir' | dir' <- adjacent dir]
  where
    single dir'
      | S.member (pos+dir+dir') cubes = norm (pos+dir+dir', -dir')
      | S.member(pos+dir') cubes = norm (pos+dir',dir)
      | otherwise = norm (pos, dir')

adjacent :: Dir -> [Dir]
adjacent dir = filter (\a -> abs a /= dir) [xP,-xP,yP,-yP,zP,-zP]

norm :: Face -> Face
norm (pos,dir)
  | dir < 0 = (pos+dir, -dir)
  | otherwise = (pos,dir)

cubeSets :: S.Set Pos -> M.Map Face (S.Set Face)
cubeSets cubes = runIdentity $ runUnionFind @_ @Face $ do
    let faces = S.fromList (S.toList cubes >>= facesForCube)
    pointed <- M.fromList <$> forM (S.toList faces) (\f -> (f,)<$> fresh f)
    forM_ (S.toList cubes) $ \pos -> do
        forM_ (unnormFacesForCube pos) $ \face -> do
            forM_ (uncurry (cubeConnections cubes) face) $ \face' -> do
                (pointed M.! norm face) `union` (pointed M.! norm face')
    os <- forM (M.toList pointed) $ \(face, uf) -> do
            o <- descriptor =<< repr uf
            pure (o, S.singleton face)
    pure (M.fromListWith S.union os)

parseCubes :: String -> S.Set Pos
parseCubes = S.fromList . map (parseCube . splitOn ",") . lines
  where
    parseCube [a,b,c] = V3 (read a) (read b) (read c)
    parseCube s = error ("bad cube" <> show s)
main :: IO ()
main = do
    inp <- parseCubes <$> readFile "input/Day18.txt"
    let airy = S.fromList $ airFaces (S.toList inp)
    print $ S.size airy
    let surfaces = cubeSets inp
    print $ maximum $ map S.size $ M.elems surfaces
