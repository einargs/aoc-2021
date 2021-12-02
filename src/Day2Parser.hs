{-# LANGUAGE OverloadedStrings, EmptyDataDeriving #-}
module Day2Parser (day2) where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor
import Control.Arrow

import Day

data Void deriving (Eq,Ord)

data Cmd
  = Forward Int
  | Down Int
  | Up Int
  deriving Show

parse :: T.Text -> [Cmd]
parse = P.parse p "" >>> (error "parse error" ||| id) where
  p = P.many $ P.try $ (dir <* C.space) <*> (L.decimal <* C.newline)
  dir :: P.Parsec Void T.Text (Int -> Cmd)
  dir = (C.string "forward" $> Forward) P.<|> (C.string "down" $> Down)
        P.<|> (C.string "up" $> Up)

process1 :: [Cmd] -> Int
process1 = foldl' (flip f) (0,0) >>> uncurry (*) where
  f (Forward i) = first (+i)
  f (Down i) = second (+i)
  f (Up i) = second (subtract i)

process2 :: [Cmd] -> Int
process2 = foldl' (flip f) ((0,0),0) >>> fst >>> uncurry (*) where
  assoc ((a,b),c) = (a,(b,c))
  f (Forward i) = (second (*i) >>> assoc >>> second (uncurry (+)) >>> first (+i)) &&& snd
  f (Down i) = second (+i)
  f (Up i) = second (subtract i)

process2' :: [Cmd] -> Int
process2' = foldl' (flip f) ((0,0),0) >>> fst >>> uncurry (*) where
  f (Forward i) = \((h,d),a) -> ((h+i,d+i*a),a)
  f (Down i) = second (+i)
  f (Up i) = second (subtract i)

day2 :: Day
day2 = Day (wrap process1) (wrap process2)
  (Just "2073315") (Just "1840311528") where
  wrap = ((T.pack <<< show) <<<) <<< (<<< parse)
