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
results nums = (firstNotMatchNum, (minimum sumList) + (maximum sumList))
  where 
    firstNotMatchNum = firstNotMatch nums
    sumList = fromMaybe [] . find (not . null) . map findSumListBy $ [3..100]
    firstNotMatch [] = 0
    firstNotMatch ees@(_:es) = let (pres, (x:_)) = splitAt 25 ees 
                                in if (find ((== x) . sum) $ pairs pres) == Nothing
                                      then x
                                      else firstNotMatch es
    findSumListBy n = fromMaybe [] . find ((== firstNotMatchNum) . sum) . chunkList n $ (take firstNotMatchIndex nums)
    firstNotMatchIndex = fromMaybe 0 . findIndex (== firstNotMatchNum) $ nums

pairs [] = []
pairs (x:xs) = map (\e -> [x, e]) xs ++ pairs xs

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n ess@(_:es) = take n ess : chunkList n es
