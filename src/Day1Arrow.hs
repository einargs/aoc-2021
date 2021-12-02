module Day1Arrow (day1) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R
import Control.Arrow

import Day

parse :: T.Text -> [Int]
parse = arr R.decimal >>> (const [] ||| (second (parse . T.strip) >>> uncurry (:)))

process1 :: [Int] -> Int
process1 = f 0 where
  f c (x:y:rs) = f (if x < y then c+1 else c) (y:rs)
  f c _ = c

process2 :: [Int] -> Int
process2 = f 0 where
  f c (w:x:y:z:rs) = f (if w+x+y < x+y+z then c+1 else c) (x:y:z:rs)
  f c _ = c

wrap :: ([Int] -> Int) -> T.Text -> T.Text
wrap f = T.pack . show . f . parse

day1 :: Day
day1 = Day (wrap process1) (wrap process2) (Just "1139") (Just "1103")
