{-# LANGUAGE OverloadedStrings #-}
module Day2 (day2) where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R

import Day

data Cmd
  = Forward Int
  | Down Int
  | Up Int
  deriving Show

parse :: T.Text -> [Cmd]
parse = fmap f . filter (not . T.null) . T.splitOn "\n" where
  f t = let [w,it] = T.splitOn " " t
            Right (i, _) = R.decimal it
             in case w of
                  "forward" -> Forward i
                  "down" -> Down i
                  "up" -> Up i
                  _ -> error "unrecognized"

process1 :: [Cmd] -> (Int, Int)
process1 (c:cs) =
  case c of
    Forward i -> (h+i,v)
    Down i -> (h,v+i)
    Up i -> (h,v-i)
  where (h,v) = process1 cs
process1 [] = (0,0)

process2 :: [Cmd] -> Int
process2 cs = h*d where
  (h,d,a) = foldl' f (0,0,0) cs
  f (h,d,a) c = case c of
                  Forward i -> (h+i,d+(a*i),a)
                  Down i -> (h,d,a+i)
                  Up i -> (h,d,a-i)

day2 :: Day
day2 = Day (wrap $ uncurry (*) . process1) (wrap process2) where
  wrap f = T.pack . show . f . parse
