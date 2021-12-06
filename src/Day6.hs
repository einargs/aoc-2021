module Day6 (day6) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Chimera as Ch

import Day

parse :: Text -> [Int]
parse = runParse $ L.decimal `P.sepBy` C.string ","

simulate :: Int -> [Int] -> [Int]
simulate 0 fs = fs
simulate day fs = simulate (day-1) $ simDay fs where
  simDay (0:fs) = 8:6:simDay fs
  simDay (f:fs) = (f-1):simDay fs
  simDay [] = []

part1 :: [Int] -> Int
part1 = length . simulate 80

-- | Takes days left and fish timer and returns offspring count.
simFish0 :: Int -> Int -> Int
simFish0 days init
  | days' <= 0 = 1
  | otherwise = simFish0 days' 6 + simFish0 days' 8 where
  days' = days - 1 - init

simNewFish :: (Word -> Word) -> Word -> Word
simNewFish f days = f' days 7 + f' days 9 where
  f' days n | days < n = 1
  f' days n = f (days - n)

memoizedSim :: Word -> [Int] -> Word
memoizedSim startDays = sum . fmap simFish' where
  simNewFish' = Ch.memoizeFix simNewFish
  simFish' init = simNewFish' (startDays - 1 - fromIntegral init)

day6 :: Day
day6 = Day (wrap part1) (wrap $ memoizedSim 256)
  (Just "351188") (Just "1595779846729") where
  wrap f = T.pack . show . f . parse
