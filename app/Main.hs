module Main where

import Data.List ((!!))
import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Day
import Day1Arrow
import Day2Parser
import Day3
import Day4
import Day5

days :: [Day]
days = [day1,day2,day3,day4,day5]

runPart :: Text -> Maybe Text -> IO ()
runPart ans mbAns = do
  TIO.putStrLn ans
  case mbAns of
    Just ans' | ans' == ans -> TIO.putStrLn "Correct"
              | otherwise -> TIO.putStrLn "Wrong"
    Nothing -> pure ()

main :: IO ()
main = do
  (dayN:partTxt:rs) <- getArgs
  let filename = case rs of
                   [n] -> n
                   _ -> "inputs/" <> dayN <> ".txt"
  txt <- TIO.readFile filename
  let dayIdx = read dayN - 1
      day = if dayIdx < length days then days !! dayIdx
                                    else error "Day not loaded"
      p1 = runPart (dayPart1 day txt) (part1Ans day)
      p2 = runPart (dayPart2 day txt) (part2Ans day)
  case partTxt of
    "part1" -> p1
    "part2" -> p2
    "both" -> p1 *> p2
    _ -> error "specify part1 or part2"
