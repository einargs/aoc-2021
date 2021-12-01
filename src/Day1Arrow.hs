module Day1Arrow (run) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as R
import Control.Arrow

parse :: T.Text -> [Int]
parse = arr R.decimal >>> (const [] ||| (second (parse . T.strip) >>> uncurry (:)))

process :: [Int] -> Int
process = f 0 where
  f c (w:x:y:z:rs) = f (if w+x+y < x+y+z then c+1 else c) (x:y:z:rs)
  f c _ = c

run :: IO ()
run = TIO.interact $ T.pack . show . process . parse
