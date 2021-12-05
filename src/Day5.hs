module Day5 (day5) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Control.Applicative ((<|>))
import Data.List (transpose, foldl', find)
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

import Day

type Point = (Int,Int)
type Lines = [(Point, Point)]
type Board = Map (Int,Int) Int

parse :: T.Text -> Lines
parse = runParse $ P.many (line <* P.optional C.newline) where
  point = P.try $ (,) <$> (L.decimal <* C.string ",") <*> L.decimal
  line = P.try $ (,) <$> (point <* C.string " -> ") <*> point

axialLines :: Lines -> Lines
axialLines = filter f where
  f ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

addPoints :: Board -> [Point] -> Board
addPoints b [] = b
addPoints m (p:ps) = addPoints (M.alter f p m) ps where
  f Nothing = Just 1
  f (Just n) = Just $ n + 1

addLines :: Lines -> Board
addLines = f M.empty where
  bwt a b | a < b = [a..b]
          | otherwise = reverse [b..a]
  addLine m (x1,y1) (x2,y2)
    | x1 == x2 = addPoints m $ [(x1,y) | y <- bwt y1 y2]
    | y1 == y2 = addPoints m $ [(x,y1) | x <- bwt x1 x2]
    | otherwise = addPoints m $ zip (bwt x1 x2) (bwt y1 y2)
  f b [] = b
  f m ((p1, p2):ls') =
    f (addLine m p1 p2) ls'

countOverlaps :: Board -> Int
countOverlaps = length . filter (>1) . M.elems

day5 :: Day
day5 = Day p1 p2 (Just "6283") (Just "18864") where
  p1 = wrap $ countOverlaps . addLines . axialLines
  p2 = wrap $ countOverlaps . addLines
  wrap f = T.pack . show . f . parse

