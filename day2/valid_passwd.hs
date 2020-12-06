#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
-- 3-10 q: cbqszqfqqqbvrrtsfq
-- 4-17 p: pppkppppppppppppwp
-- 11-12 n: nnnnnnnnnnnn

import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = ((Int, Int), Char, String)
type Answer = (Int, Int)

main = interact $ showResults . results . map parseRaw . lines

showResults :: Answer -> String
showResults (n1, n2) = "Part I answer: " ++ show n1 ++ ".\nPart II answer: " ++ show n2 ++ ".\n"

results :: [Entry] -> Answer
results es = (length $ filter lengthValid es, length $ filter posValid es)

parseRaw :: String -> Entry
parseRaw s = let ws = words s in (parseIntPair $ head ws, head $ ws !! 1, last ws)
  where
    parseIntPair :: String -> (Int, Int)
    parseIntPair s = let ns = map (read . T.unpack) . T.splitOn (T.pack "-") $ (T.pack s)
                      in (head ns, last ns)
    
lengthValid :: Entry -> Bool
lengthValid ((l, u), c, s) = let n = length . filter (== c) $ s in n >= l && n <= u

posValid :: Entry -> Bool
posValid ((p1, p2), c, s) = (s !! (p1 - 1) == c) /= (s !! (p2 - 1) == c)

