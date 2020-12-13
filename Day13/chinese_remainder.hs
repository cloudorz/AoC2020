#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
-- https://zh.wikipedia.org/wiki/中国剩余定理
--
import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = (Int, [String])
type Answer = (Int, Int)

testInputs :: Entry
testInputs = (939, ["7", "13", "x", "x", "59", "x", "31", "19"])

main = interact $ showResult . results . parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: [String] -> Entry
parseRaw lines = (read $ head lines, split "," $ last lines)

results :: Entry -> Answer
results (ts, xs) = (p1, p2)
  where
    p1 = uncurry (*) . minimumBy (\(n1, _) (n2, _) -> compare n1 n2) . map (\n -> (n - mod ts n, n)) $ ms
    p2 = mod (sum $ zipWith3 (\x y z -> x*y*z) sMi sti ai) sM
    busSchedules :: [(Int, Int)]
    busSchedules = (fmap . fmap) read . filter ((/= "x") . snd) $ zip [0..] xs
    ms = snd . unzip $ busSchedules
    sM = product ms
    sMi = map (div sM) ms
    sti = zipWith modInv sMi ms
    ai = map (\(p, m) -> mod (m - p) m) busSchedules

modInv m p
    | mod m p == 1 = 1
    | otherwise = let m' = mod m p in (p - div p m') * modInv (mod p m') p `mod` p

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
