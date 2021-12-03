{-# LANGUAGE OverloadedStrings, EmptyDataDeriving, LambdaCase #-}
module Day3 (day3) where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Arrow

import Day

data Void deriving (Eq,Ord)

parseBin :: String -> Int
parseBin = T.pack >>> P.parse p "" >>> (error "parse error" ||| id) where
  p :: P.Parsec Void T.Text Int
  p = L.binary

invert :: String -> String
invert = fmap f where
  f '1' = '0'
  f '0' = '1'

part1 :: T.Text -> T.Text
part1 = T.pack . show . g . fmap f . T.transpose . T.lines . T.strip where
  f t = let o = T.count "1" t
            z = T.count "0" t
            in case compare o z of
                  GT -> '1'
                  LT -> '0'
                  EQ -> error "not defined"
  g i = parseBin i * parseBin i' where
    i' = invert i

part2 :: T.Text -> T.Text
part2 = T.pack . show . f . T.lines . T.strip where
  f ts = ox*co where
    ox = loc 0 oxCrit ts
    co = loc 0 coCrit ts
  mkCrit :: (Ordering -> Char) -> [Char] -> Char
  mkCrit c cs = c (compare o z) where
    t = T.pack cs
    o = T.count "1" t
    z = T.count "0" t
  oxCrit = mkCrit $ \case
    LT -> '0'
    _ -> '1'
  coCrit = mkCrit $ \case
    LT -> '1'
    _ -> '0'
  loc :: Int -> ([Char] -> Char) -> [T.Text] -> Int
  loc _ _ [t] = parseBin $ T.unpack t
  loc i crit ts | i <= T.length (head ts) = loc (i+1) crit ts' where
    getC t = T.index t i
    desiredBit = crit $ getC <$> ts
    ts' = filter ((== desiredBit) . getC) ts

day3 :: Day
day3 = Day part1 part2 (Just "3958484") (Just "1613181")


