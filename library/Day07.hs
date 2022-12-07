{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Data.Char (isLetter, isSpace)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (void, guard)
import Control.Monad.Trans.Writer.CPS
import Data.List (sort)

input :: IO T.Text
input = T.readFile "input/Day07.txt"

type Path = T.Text
data Tree = Dir { dirName :: T.Text, files :: M.Map Path Int, subdirs :: M.Map Path Tree }
           deriving (Show, Eq)

type Parser = Parsec Void T.Text
pPath :: Parser T.Text
pPath = lexeme $ string "/" <|> takeWhile1P (Just "path") (\x -> x == '.' || isLetter x)
pDirEntry :: Parser (Either T.Text (Int,T.Text))
pDirEntry = try $ fmap Left pDir <|> fmap Right pFile
  where
    pDir = lexeme (string "dir") *> pPath
    pFile = do
      size <- lexeme decimal
      name <- pPath
      pure (size, name)

pLS :: Parser [Either T.Text (Int,T.Text)]
pLS = do
  lexeme $ void $ string "$ ls"
  many pDirEntry
parseUp :: Parser ()
parseUp = lexeme $ void $ string "$ cd .."
parseCD :: Parser Path
parseCD = try $ do
  _ <- try $ lexeme $ string "$ cd"
  path <- pPath <|> lexeme (string "..")
  guard (path /= "..")
  pure path

lexeme :: Parser a -> Parser a
lexeme m = m <* takeWhileP (Just "whitespace") isSpace
pSubDir :: Parser Tree
pSubDir = do
  path <- parseCD
  os <- many (fmap Right pLS <|> fmap Left pSubDir )
  eof <|> parseUp
  pure $ Dir path (theFiles os) (theSubs os)
  where
    theFiles ls = M.fromList [ (b,a) | Right lsEntries <- ls, Right (a,b) <- lsEntries ]
    theSubs ls = M.fromList [ (dirName l, l) | Left l <- ls ]

largeDirectories :: Tree -> [(Int, Path)]
largeDirectories t = execWriter (go t)
  where
    go :: Tree -> Writer [(Int, Path)] Int
    go tree = do
       subs <- mapM go (subdirs tree)
       let locals = sum $ files tree
           out = sum subs + locals
       tell [(out, dirName tree)]
       pure out

parser :: Parser a -> T.Text -> a
parser p = either (error . errorBundlePretty) id . parse p ""
main :: IO ()
main = do
  inp <- input
  let dirs = largeDirectories $  parser pSubDir inp
  print $ sum $ filter (<= 100000) $ map fst dirs
  let root = fst $ head (filter ((== "/") . snd) dirs)
  let left =  30000000 - (70000000 - root)
      sortDirs = dropWhile (< left) $ sort $ map fst dirs
  print $ head sortDirs
