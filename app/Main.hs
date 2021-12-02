module Main where

import Data.List ((!!))
import System.Environment
import qualified Data.Text.IO as TIO

import Day
import Day1Arrow
import Day2

days :: [Day]
days = [day1,day2]

main :: IO ()
main = do
  [dayN, partTxt] <- getArgs
  let day = days !! (read dayN - 1)
      part = case partTxt of
               "part1" -> dayPart1 day
               "part2" -> dayPart2 day
               _ -> error "specify part1 or part2"
  txt <- TIO.readFile $ "inputs/" <> dayN <> ".txt"
  TIO.putStrLn $ part txt
