#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
-- BFFFFBFLLL
-- BFBBFBFLLR
-- ....
-- FBFBBBBLRR

import Data.Foldable (foldl')
import Data.List ((\\))

main = interact $ showResult . results . map seatIDFromStr . lines

showResult :: (Int, Int) -> String
showResult (hid, mid) = "The highest seat ID: " ++ show hid ++ ".\nMy seat ID: " ++ show mid ++ ".\n"

results :: [Int] -> (Int, Int)
results ns = let mi = minimum ns 
                 mx = maximum ns
             in  (mx, head $ [mi..mx] \\ ns)
                 

seatIDFromStr :: String -> Int
seatIDFromStr ts = let (p, s) = splitAt 7 ts 
                   in rowFromStr p 'B' * 8 + rowFromStr s 'R'

rowFromStr :: String -> Char -> Int
rowFromStr s r = round $ l * (foldl' strToNum 0 $ zip s [1..])
  where 
    strToNum v (c, i) = if c == r then v + 2**(-i) else v
    l = 2**(fromIntegral $ length s)

