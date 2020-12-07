#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Data.Maybe

type Entry = Int
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = read

results :: [Entry] -> Answer
results yrs = (pResult pairs, pResult triple)
  where
    pResult f = fromMaybe 0 . fmap product . find ((== 2020) . sum) . f $ sort yrs
    pairs [] = []
    pairs (x:xs) = map (\e -> [x, e]) xs ++ pairs xs
    triple [] = []
    triple (x:y:xs) = map (\e -> [x,y,e]) xs ++ triple (y:xs)
