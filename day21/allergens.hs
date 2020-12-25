#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import qualified Data.Text as T

type Entry = ([String], [String])
type Answer = (Int, String)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw s =
  let (p1:p2:[]) = split " (contains " s
  in (words p1, split ", ".init $ p2)

results :: [Entry] -> Answer
results es =
  let allAllergens = nub.concat.snd $ unzip es
      allIngredients = fst $ unzip es
      genDientRs = zip allAllergens $ map findR allAllergens
      genDientMap = findGDPair [] genDientRs
      genDients = snd.unzip $ genDientMap
  in (length.filter (not.(`elem` genDients)) $ concat allIngredients
     , intercalate ",".snd.unzip.sort $ genDientMap)
  where
    findGDPair ps [] = ps
    findGDPair ps rs =
      let (pps, prs) = partition ((== 1).length.snd) rs
          newPs = (fmap.fmap) head pps
          matchedIngredients = snd $ unzip newPs
          newRs = (fmap.fmap) (\\ matchedIngredients) prs
      in findGDPair (ps++newPs) (filter (not.null.snd) newRs)
    findR gen = foldl1 intersect.fst.unzip.filter (elem gen.snd) $ es

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
