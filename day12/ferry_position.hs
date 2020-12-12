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
import Data.Bifunctor
import Data.Tuple

type Entry = (Char, Int)
type Answer = ((Int, Int), (Int, Int))

testInputs = ["F10", "N3", "F7", "R90", "F11"]

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw s = (head s, read . drop 1 $ s) 

results :: [Entry] -> Answer
results es = (excute 'E' (0, 0) es, excute2 (10, 1) (0, 0) es)
  where
    excute2 waypoint pos [] = pos
    excute2 waypoint pos (one:actions) = if fst one `elem` ['N', 'S', 'E', 'W'] 
                                            then excute2 (move (fst one) (snd one) waypoint) pos actions
                                            else if fst one `elem` ['L', 'R'] 
                                                    then excute2 (rotate (fst one) (snd one) waypoint) pos actions
                                                    else excute2 waypoint (mergePos (snd one) waypoint pos) actions
    excute direction pos [] = pos
    excute direction pos (one:actions) = if fst one `elem` ['N', 'S', 'E', 'W'] 
                                            then excute direction (move (fst one) (snd one) pos) actions 
                                            else case one of 
                                                   ('L', v) -> excute (degreeToDirection (directionToDegree direction + v)) pos actions
                                                   ('R', v) -> excute (degreeToDirection (directionToDegree direction - v)) pos actions
                                                   ('F', v) -> excute direction (move direction v pos) actions
    move d v pos = case (d, v) of 
                     ('N', v) -> second (+ v) pos
                     ('S', v) -> second (+ (-v)) pos
                     ('E', v) -> first (+ v) pos
                     ('W', v) -> first (+ (-v)) pos
    mergePos k (x, y) (e, n) = (k*x + e, k*y + n)
    rotate d v pos = case (d, v) of
                       ('R', 90) -> second (* (-1)) $ swap pos
                       ('R', 270) -> first (* (-1)) $ swap pos
                       (_, 180) -> bimap (* (-1)) (* (-1)) pos
                       ('L', 90) -> first (* (-1)) $ swap pos
                       ('L', 270) -> second (* (-1)) $ swap pos


-- N: 0, W: 90, S: 180, E: 270
mapping = [('N', 0), ('W', 90), ('S', 180), ('E', 270)]
directionToDegree :: Char -> Int
directionToDegree d = fromMaybe 0 . fmap snd . find ((== d) . fst) $ mapping

degreeToDirection :: Int -> Char
degreeToDirection p = fromMaybe 'E' . fmap fst . find ((== mod p 360) . snd) $ mapping
