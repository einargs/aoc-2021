module Day6Manual (day6) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V

import Day

parse :: Text -> [Word]
parse = runParse $ word `P.sepBy` C.string "," where
  word = fromIntegral <$> L.decimal

-- | For a new fish with `days` left, gives the number of fish resulting.
simNewFish :: (Word -> Word) -> Word -> Word
simNewFish f days = f' days 7 + f' days 9 where
  f' days n | days < n = 1
  f' days n = f (days - n)

simulate :: Word -> [Word] -> Word
simulate startDays = sum . fmap simFish where
  simFish init = simNewFish' (startDays - 1 - init)
  simNewFish' = simNewFish $ (lazyVec V.!) . fromIntegral
  lazyVec = V.generate (fromIntegral startDays) (simNewFish' . fromIntegral)

day6 :: Day
day6 = Day (simFor 80) (simFor 256) (Just "351188") (Just "1595779846729") where
  simFor days = T.pack . show . simulate days . parse
