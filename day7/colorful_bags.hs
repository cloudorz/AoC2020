#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Data.Bifunctor (first, second)
import Data.Maybe
import Data.Char
--import Text.Regex.TDFA

type Entry = (String, String)
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw s = let chopStrings = split "s contain " s 
              in (head chopStrings, last chopStrings)

myBagDesc = "shiny gold bag"
emptyBagDesc = "no other bags."

results :: [Entry] -> Answer
results es = (myBagContianerCount es [myBagDesc], bagCountBy myBagDesc)
  where 
    myBagContianerCount _ [] = 0
    myBagContianerCount xs ys = let (xs', ys') = foldr (classify ys) ([], []) xs 
                                      in length ys' + myBagContianerCount xs' ys'
    classify cs a b = let (h, bd) = a 
                       in if any (`isInfixOf` bd) cs 
                             then second (fst a :) b 
                             else first (a :) b
    bagCountBy name = let body = findBody name es 
                      in if body == emptyBagDesc
                            then 0 
                            else sum . map (\(n, s) -> n + n * bagCountBy s) . parseBody $ body
    findBody s = fromMaybe emptyBagDesc . fmap snd . find (\(name, _) -> name `isInfixOf` s)

parseBody :: String -> [(Int, String)]
parseBody = map (\(x:xs) -> (digitToInt x, xs)) . split ", "

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
