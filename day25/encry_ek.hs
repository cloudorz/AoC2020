#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import qualified Data.Text as T
import Data.Maybe (fromJust)

type Entry = Int
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Door's key: " ++ show p1 ++ ".\nCard's key: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = read

results :: [Entry] -> Answer
results (cpk:dpk:[]) =
  let cls = getLoopSize cpk
      dls = getLoopSize dpk
  in (encryptionKey cpk dls, encryptionKey dpk cls)
  where
    sn = 7
    m = 20201227
    getLoopSize pk = fromJust.findIndex (== pk) $ iterate ((flip mod m).(* sn)) 1
    encryptionKey pk ls = (iterate ((flip mod m).(* pk)) 1) !! ls

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
