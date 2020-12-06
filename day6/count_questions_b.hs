#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
-- agvfm
-- jvogqfiau
-- vdaogf

-- c
-- gqd

-- sgfpy
-- sgfbpl
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Data.Bifunctor (bimap)

type Entry = [String]
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = words

results :: [Entry] -> Answer
results = bimap sum sum . unzip . map (\e -> (length . nub . concat $ e, length . intersects $ e))
  where 
    intersects e = let h = head e in foldl' intersect h e

