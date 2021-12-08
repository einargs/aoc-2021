{-# LANGUAGE LambdaCase #-}
module Day8 (day8) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, permutations)
import Debug.Trace

import Day

data Seg = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show, Enum)

type Disp = S.Set Seg

data Entry = Entry {sigIns :: [Disp], sigOuts :: [Disp]}
  deriving Show

convChar :: Char -> Seg
convChar = \case
  'a' -> A
  'b' -> B
  'c' -> C
  'd' -> D
  'e' -> E
  'f' -> F
  'g' -> G

convDisplay :: String -> Disp
convDisplay = S.fromList . fmap convChar

segToIndex :: Seg -> Int
segToIndex = fromEnum

parse :: Text -> [Entry]
parse = runParse $ entry `P.sepEndBy` C.newline where
  entry = do
    ins <- sig `P.sepEndBy` C.hspace
    C.string "| "
    outs <- sig `P.sepBy` C.hspace
    pure $ Entry ins outs
  sig = fmap convDisplay $ P.some $ P.oneOf ['a'..'g']

-- | Count occurances of 1,4,7, and 8 in outputs. 1=2,4=4,7=3,8=7.
part1 :: [Entry] -> Int
part1 = sum . fmap f where
  f (Entry _ outs) = length $ filter g outs where
    g d = S.size d `elem` [2,4,3,7]

segments :: [Disp]
segments = convDisplay <$> ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg",
  "abdefg", "acf", "abcdefg", "abcdfg"]

type Key = M.Map Disp Int

trueKey :: Key
trueKey = M.fromList $ zip segments [0..]

segmentSet :: S.Set Disp
segmentSet = S.fromList segments

checkSolution :: [Disp] -> (Seg -> Seg) -> Bool
checkSolution ds f = segmentSet == S.fromList (fmap (S.map f) ds)

allSolutions :: [Seg -> Seg]
allSolutions = convert <$> permutations [A .. G] where
  convert :: [Seg] -> Seg -> Seg
  convert xs x = xs !! segToIndex x

deduceKey :: Entry -> Key
deduceKey (Entry ins _) = key where
  check = checkSolution ins
  Just solution = find check allSolutions
  convert disp = S.map solution disp
  key = M.fromList $ fmap f ins where
    f d = (d, trueKey M.! convert d)

calculateOutput :: Entry -> Int
calculateOutput e@(Entry _ out) = mkNum $ fmap (key M.!) out where
  key = deduceKey e
  mkNum :: [Int] -> Int
  mkNum = read . (>>= show)

part2 :: [Entry] -> Int
part2 = sum . fmap calculateOutput

day8 :: Day
day8 = Day (wrap part1) (wrap part2) (Just "349") (Just "1070957") where
  wrap f = T.pack . show . f . parse
