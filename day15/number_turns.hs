#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import Data.Maybe

type Entry = Int
type Answer = (Int, Int)

inputs :: [Int]
inputs = [19, 0, 5, 1, 10, 13]

main = print $ turns2 30000000 inputs -- interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = undefined

results :: [Entry] -> Answer
results  = undefined

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack

turns th pres = turns_ (length pres) $ reverse pres
  where turns_ count pres
          | count == th = head pres
          | otherwise = let indices = take 2 . findIndices (== head pres) $ pres
                        in if length indices == 2
                             then turns_ (succ count) (((indices !! 1) - (indices !! 0)):pres)
                             else turns_ (succ count) (0:pres)

type Record = IM.IntMap (Int, Int)
turns2 nth pres = turns_ (IM.fromList kvList) (last kvList)
  where
    kvList = zip pres $ map ((,) 0) [1..]
    turns_ record (lastN, (p, l)) = if l == nth
                                      then lastN
                                      else let newN = if p > 0 then l - p else 0
                                           in case IM.lookup newN record of
                                             Nothing -> turns_ (IM.insert newN (0, l+1) record) (newN, (0, l+1))
                                             Just (p', l') -> turns_ (IM.insert newN (l', l+1) record) (newN, (l', l+1))
