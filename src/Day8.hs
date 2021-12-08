module Day8 (day8) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Debug.Trace

import Day

data Entry = Entry {sigIns :: [Text], sigOuts :: [Text]}
  deriving Show

parse :: Text -> [Entry]
parse = runParse $ entry `P.sepEndBy` C.newline where
  entry = do
    ins <- sig `P.sepEndBy` C.hspace
    C.string "| "
    outs <- sig `P.sepBy` C.hspace
    pure $ Entry ins outs
  sig = fmap T.pack $ P.some $ P.oneOf ['a'..'g']

-- | Count occurances of 1,4,7, and 8 in outputs. 1=2,4=4,7=3,8=7.
part1 :: [Entry] -> Int
part1 = sum . fmap f where
  f (Entry _ outs) = length $ filter g outs where
    g t = T.length t `elem` [2,4,3,7]

segments :: [Text]
segments = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg",
  "acf", "abcdefg", "abcdfg"]

type Key = M.Map Text Int

trueKey :: Key
trueKey = M.fromList $ zip segments [0..]

deduceKey :: Entry -> Key
deduceKey (Entry ins _) = M.mapKeys (T.map encode) trueKey where
  Just eight = find ((==7) . T.length) ins
  encode :: Char -> Char
  encode c = T.index eight i where
    Just i = T.findIndex (==c) "abcdefg"

calculateOutput :: Entry -> Int
calculateOutput e@(Entry _ out) = traceShow (key, e) $ mkNum $ fmap (key M.!) out where
  key = deduceKey e
  mkNum :: [Int] -> Int
  mkNum = read . (>>= show)

part2 :: [Entry] -> Int
part2 = sum . fmap calculateOutput

day8 :: Day
day8 = Day (wrap part1) (wrap part2) Nothing Nothing where
  wrap f = T.pack . show . f . parse
