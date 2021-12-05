{-# LANGUAGE EmptyDataDeriving, DeriveAnyClass #-}
module Day where

import Data.Text (Text)
import qualified Text.Megaparsec as P

data Day = Day
  { dayPart1 :: Text -> Text
  , dayPart2 :: Text -> Text
  , part1Ans :: Maybe Text
  , part2Ans :: Maybe Text
  }

data Void deriving (Eq,Ord,Show, P.ShowErrorComponent)

type Parser = P.Parsec Void Text
