module Main where

import Data.List ((!!))
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Day
import Day1Arrow
import Day2Parser

days :: [Day]
days = [day1,day2]

runPart :: Text -> Maybe Text -> IO ()
runPart ans mbAns = do
  TIO.putStrLn ans
  case mbAns of
    Just ans' | ans' == ans -> TIO.putStrLn "Correct"
              | otherwise -> TIO.putStrLn "Wrong"
    Nothing -> pure ()

main :: IO ()
main = do
  [dayN, partTxt] <- getArgs
  txt <- TIO.readFile $ "inputs/" <> dayN <> ".txt"
  let day = days !! (read dayN - 1)
      p1 = runPart (dayPart1 day txt) (part1Ans day)
      p2 = runPart (dayPart2 day txt) (part2Ans day)
  case partTxt of
    "part1" -> p1
    "part2" -> p2
    "both" -> p1 *> p2
    _ -> error "specify part1 or part2"
