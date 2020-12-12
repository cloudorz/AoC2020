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

-- E: (1, 0) , W: (-1, 0), N: (0, 1), S: (0, -1)
--
results :: [Entry] -> Answer
results es = (excute False (1, 0), excute True (10, 1))
  where
    excute p origin = excute_ origin (0, 0) es
      where
        excute_ origin pos [] = pos
        excute_ origin pos (one:actions) = let (px, py) = case one of
                                                            ('F', v) -> (origin, (scaleP v origin `plusP` pos))
                                                            ('L', v) -> ((rotateP (-v) origin), pos)
                                                            ('R', v) -> ((rotateP v origin), pos)
                                                            _ -> (if p then first else second) (plusP (toPair one)) (origin, pos)
                                            in excute_ px py actions
    toPair c = case c of 
                 ('N', v) -> (0, v)
                 ('S', v) -> (0, -v)
                 ('E', v) -> (v, 0)
                 ('W', v) -> (-v, 0)
    scaleP k (x, y) = (k*x, k*y)
    plusP (x, y) (x1, y1) = (x+x1, y+y1)
    -- x2 = cos * x1 + sin * y1, y2 = cos * y1 - sin * x1
    rotateP :: Int -> (Int, Int) -> (Int, Int)
    rotateP degree (x, y) = let p = fromIntegral degree/360*2*pi 
                             in (round(cos p * fromIntegral x + sin p * fromIntegral y), round(cos p * fromIntegral y - sin p * fromIntegral x))
