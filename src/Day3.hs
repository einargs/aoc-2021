{-# LANGUAGE OverloadedStrings, EmptyDataDeriving, LambdaCase #-}
module Day3 (day3) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (transpose, foldl')
import Control.Arrow

import Day

data Void deriving (Eq,Ord)

parseBin :: T.Text -> Int
parseBin = P.parse p "" >>> (error "parse error" ||| id) where
  p :: P.Parsec Void T.Text Int
  p = L.binary

invert :: String -> String
invert = fmap f where
  f '1' = '0'
  f '0' = '1'

cmp1to0 :: [Char] -> Ordering
cmp1to0 cs = compare (foldl' f 0 cs) 0 where
  -- positive means more ones; negative means more zeroes
  f c '1' = c+1
  f c '0' = c-1

part1 :: T.Text -> T.Text
part1 = T.pack . show . g . fmap f . transpose
        . fmap T.unpack . T.lines . T.strip where
  f cs = case cmp1to0 cs of
           GT -> '1'
           LT -> '0'
           EQ -> error "not defined"
  g is = parseBin i * parseBin i' where
    i = T.pack is
    i' = T.pack $ invert is

part2 :: T.Text -> T.Text
part2 = T.pack . show . f . T.lines . T.strip where
  f ts = ox*co where
    ox = loc 0 oxCrit ts
    co = loc 0 coCrit ts
  oxCrit = cmp1to0 >>> \case
    LT -> '0'
    _ -> '1'
  coCrit = cmp1to0 >>> \case
    LT -> '1'
    _ -> '0'
  loc :: Int -> ([Char] -> Char) -> [T.Text] -> Int
  loc _ _ [t] = parseBin t
  loc i crit ts | i <= T.length (head ts) = loc (i+1) crit ts' where
    getC t = T.index t i
    desiredBit = crit $ getC <$> ts
    ts' = filter ((== desiredBit) . getC) ts

day3 :: Day
day3 = Day part1 part2 (Just "3958484") (Just "1613181")
