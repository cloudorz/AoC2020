#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = [Int]
type Answer = (Int, Int)

rules :: [[Int]]
rules = [ -- | all range connect togater is [28..974]
  [29..917] ++ [943..952],  -- departure location
  [50..875] ++ [884..954],  -- departure station
  [41..493] ++ [503..949],  -- departure platform
  [50..867] ++ [875..966],  -- departure track
  [30..655] ++ [679..956],  -- departure date
  [46..147] ++ [153..958],  -- departure time
  [50..329] ++ [344..968],  -- arrival location
  [42..614] ++ [623..949],  -- arrival station
  [35..849] ++ [860..973],  -- arrival platform
  [42..202] ++ [214..959],  -- arrival track
  [38..317] ++ [329..968],  -- class
  [44..530] ++ [539..953],  -- duration
  [28..713] ++ [727..957],  -- price
  [30..157] ++ [179..966],  -- route
  [38..114] ++ [136..969],  -- route
  [45..441] ++ [465..956],  -- row
  [44..799] ++ [824..951],  -- train
  [41..411] ++ [437..953],  -- type
  [39..79] ++ [86..969],    -- wagon
  [48..306] ++ [317..974]]  -- zone

ticket = [
  191,89,73,
  139,71,103,
  109,53,97,
  179,59,67,
  79,101,113,
  157,61,107,
  181,137]

main = interact $ showResult . results . map parseRaw . drop 25 . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: "
  ++ show p1
  ++ ".\nPart II answer: "
  ++ show p2
  ++ ".\n"

parseRaw :: String -> Entry
parseRaw = map read . split ","

results :: [Entry] -> Answer
results es =
  (sum . concat . map (filter (\n -> n < 28 || n > 974)) $ es
  , product . map snd . filter ((<= 6) . fst) $ type2MyTicketVList)
  where
    type2MyTicketVList = (fmap . fmap) ((ticket !!) . pred) type2ColList
    type2ColList = typeMapCol (map intList2Bin matrix) []
    typeMapCol ns ps = case find ((== 1) . popCount . snd) (zip [0..] ns) of
       Nothing -> ps
       Just (index, n) -> typeMapCol (map (.&. complement n) ns)
                                     ((index+1, 20 - lg2 n):ps)
    validTickets = filter (all (\n -> n >= 28 && n <= 974)) es
    matrix = transpose . map
      (\cols -> map (fromEnum . (<= 0) . length . (nub cols \\)) rules)
      $ transpose validTickets

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack

lg2 :: Int -> Int
lg2 = round . logBase 2 .fromIntegral

intList2Bin :: [Int] -> Int
intList2Bin = foldl' (\b c -> 2*b +c) 0
