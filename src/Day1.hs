{-# LANGUAGE OverloadedStrings #-}
module Day1 (run) where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R
import Control.Arrow

forceInt :: T.Text -> Int
forceInt t = case R.decimal t of
               Right (i, _) -> i
               Left err -> error err

forceInt' :: T.Text -> Int
forceInt' = R.decimal >>> (error ||| fst)

parse :: T.Text -> [Int]
parse = fmap forceInt . filter (not . T.null) . T.splitOn "\n"

chunk :: [Int] -> [Int]
chunk (x:y:z:rs) = (x+y+z):chunk (y:z:rs)
chunk _ = []

process0 :: [Int] -> Int
process0 = fst . foldl' f (0, Nothing) where
  f (c, Just p) i = (if i > p then c+1 else c, Just i)
  f (c, Nothing) i = (c, Just i)

process1 :: [Int] -> Int
process1 (x:xs) = fst $ foldl' f (0, x) xs where
  f (c, p) i = (if i > p then c+1 else c, i)

process2 :: [Int] -> Int
process2 (x:xs) = fst $ foldl' (flip f) (0, x) xs where
  f i = (\(c,p) -> if i>p then c+1 else c) &&& const i

run :: IO ()
run = TIO.interact $ T.pack . show . process1 . chunk . parse
