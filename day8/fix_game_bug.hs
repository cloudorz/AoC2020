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
import Control.Monad
import Data.Bifunctor
import Data.Tuple

data Instruction = Nop Int|Acc Int|Jmp Int deriving Show

type Entry = Instruction
type Answer = (Int, Int)

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw s = case words s of
               ("jmp":x:[]) -> Jmp $ read $ pre x
               ("acc":x:[]) -> Acc $ read $ pre x
               ("nop":x:[]) -> Nop $ read $ pre x 
               xs -> error ("Not support instruction." ++ show xs)
  where
    pre x = fromMaybe x . stripPrefix "+" $ x

results :: [Entry] -> Answer
results insSet = swap . bimap (fst . head) (fst . head) . mapAccumL f [] . map stepsFrom $ allInsSetList
  where
    f a steps = let firstDupIndex = findDupIndex steps 
                 in if firstDupIndex == Nothing 
                       then (last steps : a, last steps)
                       else (a, (steps !!) . pred . fromJust $ firstDupIndex)
    allInsSetList = insSet : map newInsSetWith changableInsList 
    newInsSetWith pe = let (ps1, (p:ps2)) = partition (\e -> fst e < fst pe) $ zip [0..] insSet 
                        in snd . unzip $ ps1 ++ ((fmap modifyIns pe):ps2)
    modifyIns ins = case ins of
                    Jmp n -> Nop n
                    Nop n -> if n == 0 then ins else Jmp n
                    _ -> error "Non swapable instruction."
    changableInsList = filter (\(_, e) -> case e of
                                         Jmp _ -> True
                                         Nop _ -> True
                                         _ -> False) $ zip [0..] insSet

    p1 = let steps = stepsFrom insSet 
          in fromMaybe 0 . fmap (fst . (steps !!) . ((+) (-1))) $ findDupIndex steps
  
stepsFrom insSet = steps 0 0
  where
    steps value pos
      | pos >= length insSet = []
      | otherwise = (value, pos) : case insSet !! pos of 
                                       Jmp n -> steps value (pos + n)
                                       Acc n -> steps (value + n) (pos + 1)
                                       Nop _ -> steps value (pos + 1)

findDupIndex :: [(Int, Int)] -> Maybe Int
findDupIndex [] = Nothing
findDupIndex (x:[]) = Nothing
findDupIndex es = search [] es
  where 
    search _ [] = Nothing
    search xs (y:ys) = let index = findIndex (\e -> snd e == snd y) xs 
                        in if index == Nothing 
                              then search (y:xs) ys 
                              else Just $ (length xs) + 1
