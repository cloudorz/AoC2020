#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
-- #......#..#.#..#..#.#..#...#...
-- ...............#..........#....
-- .....#...........#......#....#.
-- ........#..#...............#...
-- .........#...#.......#.#..#...#
-- ..#..#......#.##..........#....
-- .#...#....#.....#.............#
-- .##.....#.........#......#..#..
--

import Data.List
import Data.Bits
import qualified Data.Text as T
import qualified Data.Foldable (foldl')

type Entry = Int
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = foldl' str2Int 0 
  where
    -- '.' means 0 '#' means 1 so "...#..#" means 0b0001001 , 9 in Int 
    str2Int b c = 2*b + if c == '#' then 1 else 0

results :: [Entry] -> Answer
results es = (treeCountWith es 3 1, product . map (uncurry $ treeCountWith es) $ pp)
  where 
    treeCountWith es r d  = snd $ foldl' (\(pos, total) e -> (pos + r, if e .&. posInt (pos + r) > 0 then total + 1 else total)) (-r, 0) (slice d 0 (length es) es)
    posInt pos = 2^(intLen - 1 - mod pos intLen)
    intLen = 31
    moveWayB = [(3, 1), (5, 1), (7, 1), (1, 2), (1, 1)]

slice :: Int -> Int -> Int -> [a] -> [a]
slice step start end = snd . unzip . filter (\(i, _) -> i >= start && i <= end && (mod (i - start) step == 0)) . zip [0..] 

