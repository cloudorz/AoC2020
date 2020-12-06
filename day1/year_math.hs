#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = Int
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = read

results :: [Entry] -> Answer
results yrs = (p1, p2)
  where
    p1 = head [y1*y2 | y1 <- yrs, y2 <- yrs, y1 + y2 == 2020]
    p2 = head [y1*y2*y3 | y1 <- yrs, y2 <- yrs, y3 <- yrs, y1 + y2 + y3 == 2020]

