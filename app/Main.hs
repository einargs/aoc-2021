module Main where

import Data.List ((!!))
import System.Environment
import qualified Data.Text.IO as TIO

import Day
import Day1Arrow
import Day2Parser

days :: [Day]
days = [day1,day2]

main :: IO ()
main = do
  [dayN, partTxt] <- getArgs
  let day = days !! (read dayN - 1)
      (partF, partAns) = case partTxt of
               "part1" -> (dayPart1 day, part1Ans day)
               "part2" -> (dayPart2 day, part2Ans day)
               _ -> error "specify part1 or part2"
  txt <- TIO.readFile $ "inputs/" <> dayN <> ".txt"
  let ans = partF txt
      correctness = case partAns of
                      Just ans' | ans' == ans -> "Correct"
                                | otherwise -> " Wrong"
                      Nothing -> ""
  TIO.putStrLn ans
  case partAns of
    Just ans' | ans' == ans -> TIO.putStrLn "Correct"
              | otherwise -> TIO.putStrLn "Wrong"
    Nothing -> pure ()

