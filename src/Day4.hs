{-# LANGUAGE OverloadedStrings #-}
module Day4 (day4) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators
import Control.Monad
import Control.Applicative ((<|>))
import Data.List (transpose, foldl', find)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Arrow
import Debug.Trace

import Day

newtype Board = Board [[Maybe Int]]
  deriving Show

parse :: T.Text -> ([Int], [Board])
parse = P.parse p "" >>> (error . P.errorBundlePretty ||| id) where
  p :: Parser ([Int], [Board])
  p = do 
    nums <- pNums
    C.newline
    C.newline
    bs <- pBoard `sepBy` C.space
    pure (nums, bs) 

  pNums :: Parser [Int]
  pNums = L.decimal `sepBy` C.string ","

  pBoard :: Parser Board
  pBoard = Board <$> P.try (count 5 (line <* C.newline))

  line :: Parser [Maybe Int]
  line = many $ C.hspace *> (Just <$> L.decimal)

checkBoard :: Board -> Bool
checkBoard (Board ns) = test ns || test (transpose ns) where
  test :: [[Maybe Int]] -> Bool
  test = any $ all (== Nothing) 

updateBoard :: Int -> Board -> Board
updateBoard i (Board ns) = Board $ fmap f <$> ns where
  f (Just v) | v == i = Nothing
  f m = m

scoreBoard :: Int -> Board -> Int
scoreBoard i (Board ns) = i * sum (catMaybes $ join ns)

part1 :: ([Int], [Board]) -> Int
part1 (n:ns, bs) =
  case find checkBoard bs' of
    Just b -> scoreBoard n b
    Nothing -> part1 (ns, bs')
  where bs' = updateBoard n <$> bs

updateBoard' :: Int -> Board -> Maybe Board
updateBoard' i b = if checkBoard b' then Nothing else Just b' where
  b' = updateBoard i b

part2 :: ([Int], [Board]) -> Int
part2 (ns, bs) = f ns Nothing bs where
  f (n:ns) m bs =
    case (bs', m') of
      ([b], _) | checkBoard b -> scoreBoard n b
      ([], Just s) -> s
      _ -> f ns (m <|> m') bs'
    where
      m' = scoreBoard n <$> find checkBoard (updateBoard n <$> bs)
      bs' = mapMaybe (updateBoard' n) bs

day4 :: Day
day4 = Day (wrap part1) (wrap part2) (Just "22680") (Just "16168") where
  wrap f = T.pack . show . f . parse
