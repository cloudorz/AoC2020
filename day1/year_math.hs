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
    -- p1 = head [y1*y2 | y1 <- yrs, y2 <- yrs, y1 + y2 == 2020]
    --p1 = head [y1*(2020 - y1) | y1 <- sortedYrs, 2020 - y1 `elem` sortedYrs]
    p1 = product $ searchPairInt 2020 sortedYrs
    --p2 = head [y1*y2*y3 | y1 <- yrs, y2 <- yrs, y3 <- yrs, y1 + y2 + y3 == 2020]
    --p2 = head [y1*y2*y3 | y1 <- sortedYrs, y2 <- sortedYrs, y3 <- sortedYrs, y1 + y2 + y3 == 2020]
    p2 = product $ searchTripleInt sortedYrs
    sortedYrs = sort yrs
    searchPairInt _ [] = [0]
    searchPairInt sumInt (x:xs) = if sumInt - x `elem` xs then [x, sumInt - x] else searchPairInt sumInt xs
    searchTripleInt xs@(x:y:_) = let subInts = filter (<= 2020 - (x + y)) xs in searchPairFor subInts (map (2020 -) subInts)
                                     where 
                                       searchPairFor _ [] = [0]
                                       searchPairFor subInts (x:xs) = let pair = searchPairInt x subInts in if length pair  >= 2 then (2020-x):pair else searchPairFor subInts xs

