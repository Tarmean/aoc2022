{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void ( Void )
import Data.Char (isSpace)
import Control.Monad (void, guard)
import qualified Control.Monad.Trans.Writer.CPS as CPS
import Control.Monad.Writer.Class
import Control.Monad.State
import GHC.Base (build)
import Data.List (foldl')
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as Lexer

class Monad m => MonadCPU m where
    wait :: m ()
    addReg :: Int -> m ()
instance MonadCPU (StateT Int (CPS.Writer [Int])) where
    wait = do
      x <- get
      tell [x]
    addReg i = wait *> wait *> modify (+i)

interpAST :: MonadCPU m => Ast -> m ()
interpAST (Addx i) = addReg i
interpAST Noop = wait

runAST :: [Ast] -> [Int]
runAST xs = CPS.execWriter . flip execStateT (0::Int) $ (mapM_ interpAST xs *> wait)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

part1 :: [Int] -> Int
part1 = sum . map (\(x,y) -> (x+1) * (y+1)) . filter (isRelevant . fst) . enumerate
  where
    isRelevant i = i `mod` 40 == 19

checkActive :: [Int] -> S.Set (Int, Int)
checkActive ls = S.fromList $ do
    (idx0, v) <- enumerate ls
    let (row, col) = idx0 `divMod` 40
    guard (col >= v && col <= v+2)
    pure (row,col)

to2D :: S.Set (Int,Int) -> [[Char]]
to2D s = [[if (r,c) `S.member` s then '#' else '.' | c <- [0..39]] | r <- [0..6]]

part2 :: [Int] -> IO ()
part2 = mapM_ putStrLn . to2D . checkActive


main :: IO ()
main = do
   print . part1 . runAST . execParse syntax =<< input
   part2 . runAST . execParse syntax =<< input

type Parser = Parsec Void T.Text
lexeme :: Parser a -> Parser a
lexeme = (<* takeWhile1P (Just "whitespace") isSpace)

number :: Parser Int
number = lexeme $ Lexer.signed (pure ()) Lexer.decimal
str :: T.Text -> Parser ()
str x = void $ lexeme $ string x

data Ast = Noop | Addx Int deriving (Eq, Ord, Show)
astP :: Parser Ast
astP = noopP <|> addxP 
  where
    noopP = Noop <$ str "noop"
    addxP = str "addx" *> (Addx <$> number)
execParse :: Parser a -> T.Text -> a
execParse p = either (error . errorBundlePretty) id . runParser p ""
syntax :: Parser [Ast]
syntax = some astP <* eof

input :: IO T.Text
input = T.readFile "input/Day10.txt"
