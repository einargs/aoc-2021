{-# LANGUAGE LambdaCase #-}
module Day8 (day8) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, permutations)
import Control.Monad (guard)
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

trueKey :: Disp -> Int
trueKey d = m M.! d where
  m = M.fromList $ zip segments [0..]

segmentSet :: S.Set Disp
segmentSet = S.fromList segments

toSol :: [Seg] -> Seg -> Seg
toSol xs x = xs !! segToIndex x

checkSolution :: [Disp] -> (Seg -> Seg) -> Bool
checkSolution ds f = segmentSet == S.fromList (fmap (S.map f) ds)

allSolutions :: [Seg -> Seg]
allSolutions = toSol <$> permutations [A .. G]

bruteDeriveSolution :: [Disp] -> Seg -> Seg
bruteDeriveSolution ds = sol where
  check = checkSolution ds
  Just sol = find check allSolutions

deriveSolution :: [Disp] -> Seg -> Seg
deriveSolution ds = fromJust $ find (checkSolution ds) $ do
  sol <- permutations [A .. G]
  let sol' = toSol sol
  eqGuard sol' 1 2
  eqGuard sol' 4 4
  eqGuard sol' 7 3
  eqGuard sol' 8 7
  pure sol'
  where
    -- Get the display with the given size
    getDisp size = fromJust $ find ((==size) . S.size) ds
    eqGuard sol trueIndex size = guard $
      S.map sol (getDisp size) == (segments !! trueIndex)

calculateOutput :: Entry -> Int
calculateOutput e@(Entry ins out) = mkNum $ key <$> out where
  sol = deriveSolution ins
  key = trueKey . S.map sol
  mkNum :: [Int] -> Int
  mkNum = read . (>>= show)

part2 :: [Entry] -> Int
part2 = sum . fmap calculateOutput

day8 :: Day
day8 = Day (wrap part1) (wrap part2) (Just "349") (Just "1070957") where
  wrap f = T.pack . show . f . parse
