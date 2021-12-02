module Day where

import Data.Text (Text)

data Day = Day {dayPart1 :: Text -> Text, dayPart2 :: Text -> Text}
