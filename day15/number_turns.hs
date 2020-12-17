#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE BangPatterns #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import qualified Data.HashTable.IO as H
import Data.Maybe

type Entry = Int
type Answer = (Int, Int)

inputs :: [Int]
inputs = [19, 0, 5, 1, 10, 13]

main = turns2 30000000 inputs >>= print -- interact $ showResult . results . map parseRaw . lines

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
                             then turns_ (succ count)
                                         (((indices !! 1) - (indices !! 0)):pres)
                             else turns_ (succ count) (0:pres)


type HashTable k v = H.BasicHashTable k v
type Record = HashTable Int Int

turns2 :: Int -> [Int] -> IO Int
turns2 nth pres = do
  record <- H.fromListWithSizeHint nth kvList
  turns_ record (last pres, 0, length pres)
  where
    kvList = zip pres [1..]
    turns_ :: Record -> (Int, Int, Int) -> IO Int
    turns_ !record (!lastN, !newN, !lastIndex) = do
      if lastIndex == nth
        then return lastN
        else do
           res <- H.lookup record newN
           let newIndex = succ lastIndex
           case res of
             Nothing -> do
               H.insert record newN newIndex
               turns_ record (newN, 0, newIndex)
             Just oldIndex -> do
               H.insert record newN newIndex
               turns_ record (newN, (newIndex - oldIndex), newIndex)
