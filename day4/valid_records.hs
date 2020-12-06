#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
--{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
--import Data.Bits
--import qualified Data.Text as T
import Text.Regex.TDFA
import Data.Char

type Entry = [String]
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = words

results :: [Entry] -> Answer
results es = (length validFieldsEntries,  length $ filter (all checkValeu) validFieldsEntries) 
  where 
    validFieldsEntries = filter (\e -> (length $ deleteBy isPrefixOf "cid:" e) >= 7) es
    checkValeu :: String -> Bool
    checkValeu e = case splitAt 4 e of 
                     ("byr:", vs) -> vs =~ "^[0-9]{4}$" && let yr = read vs in yr >= 1920 && yr <= 2002
                     ("iyr:", vs) -> vs =~ "^[0-9]{4}$" && let yr = read vs in yr >= 2010 && yr <= 2020
                     ("eyr:", vs) -> vs =~ "^[0-9]{4}$" && let yr = read vs in yr >= 2020 && yr <= 2030
                     ("hgt:", vs) -> vs =~ "^([0-9]{2}in|[0-9]{3}cm)$" && if "cm" `isSuffixOf` vs then read (take 3 vs) `elem` [150..193] else read (take 2 vs) `elem` [59..76]
                     ("hcl:", vs) -> vs =~ "^#[0-9a-f]{6}$"
                     ("ecl:", vs) -> vs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                     ("pid:", vs) -> vs =~ "^[0-9]{9}$"
                     _ -> True

