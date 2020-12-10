#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Data.Bifunctor

type Entry = Int
type Answer = (Int, Int)

testInput = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = read

results :: [Entry] -> Answer
results es = (p1 joltList, p2 joltList)
  where
    p1 = uncurry (*) . bimap length length . partition (== 1)
    p2 = product . map (combineCountWith3 . pred . sum) . filter ((== 1) . head) . group
    joltList = (3 :) . fst . foldr (\c (xs, cn) -> ((c - cn) : xs, c)) ([], 0) . reverse . sort $ es
    combineCountWith3 n = 2^n - (sum . map (cb n) $ [0..(n-3)])

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack

cb :: Int -> Int -> Int
cb n k = if k >= 0 then div (product [1..n]) ((product [1..(n-k)])*(product [1..k])) else 0
