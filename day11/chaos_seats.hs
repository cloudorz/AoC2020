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

type Entry = String
type Answer = (Int, Int)

testInputs = ["L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL", "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"] 

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = id

results :: [Entry] -> Answer
results allSeats = (countOccupiedSeats $ findFixedPoint allSeatStateMapListA, countOccupiedSeats $ findFixedPoint allSeatStateMapListB)
  where
    countOccupiedSeats = sum . map (length . (filter (== '#')))
    allSeatStateMapListA = iterate (f 4 shortLight) allSeats
    allSeatStateMapListB = iterate (f 5 light) allSeats
    f n lightF as = map (\(row, es) -> map (\(col, state) -> case state of
                                                  '.' -> state
                                                  'L' -> if isAdjacentEmpty (radar lightF (row, col) as) then '#' else state
                                                  '#' -> if isAdjacentOccupied n (radar lightF (row, col) as) then 'L' else state) $ zip [0..] es) $ zip [0..] as
    isAdjacentEmpty = (>= 8) . length . filter (== 'L')
    isAdjacentOccupied n = (>= n) . length . filter (== '#')

up = first pred
down = first succ
left = second pred
right = second succ
upLeft = bimap pred pred
downLeft = bimap succ pred
upRight = bimap pred succ
downRight = bimap succ succ
notMove = id

stateAt as (row, col) = (as !! row) !! col

radar f origin as = map ($ as) . map ($ origin) . map f $ [up, down, left, right, upLeft, downLeft, upRight, downRight]

shortLight df origin as = let (row, col) = df origin
                              maxRows = length as 
                              maxCols = length $ head as
                          in if row < maxRows && row >= 0 && col >= 0 && col < maxCols 
                                then (let state = stateAt as (row, col) 
                                       in if state == '.' then 'L' else state) else 'L'
light df origin as = light_ origin
    where
      maxRows = length as
      maxCols = length $ head as
      light_ origin = let (row, col) = df origin 
                          in if row < maxRows && row >= 0 && col >= 0 && col < maxCols 
                                then (let state = stateAt as (row, col) 
                                       in if state == '.' then light_ (row, col) else state) else 'L'
findFixedPoint [] = []
findFixedPoint (x:[]) = x
findFixedPoint (x:y:xs) = if x == y then x else findFixedPoint xs 
