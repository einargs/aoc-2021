module Day7 (day7) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector.Unboxed as V
import Data.List (foldl1')

import Day

parse :: Text -> V.Vector Int
parse = V.fromList . runParse (L.decimal `P.sepBy` C.string ",")

-- | Solve using a method of calculating fuel cost for a position.
-- Cost calc takes the proposed position and the current position.
solveWith :: (Int -> Int -> Int) -> V.Vector Int -> Int
solveWith costCalc vs = foldl1' min $ movesForPos <$> [start..end] where
  start = V.minimum vs
  end = V.maximum vs 
  movesForPos :: Int -> Int
  movesForPos p = V.foldl' f 0 vs where
    f acc i = acc + costCalc p i

part1 :: V.Vector Int -> Int
part1 = solveWith $ \p i -> abs (p - i)

part2 :: V.Vector Int -> Int
part2 = solveWith $ \p i ->
  let d = abs (p - i) in (d * (d + 1)) `div` 2

day7 :: Day
day7 = Day (wrap part1) (wrap part2) (Just "352254") (Just "99053143") where
  wrap f = T.pack . show . f . parse
