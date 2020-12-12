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
import Data.Graph
import Data.Tree
import qualified Data.Sequence as S
import Data.Foldable

data Instruction = Nop Int|Acc Int|Jmp Int deriving Show

type Entry = Instruction
type Answer = (Int, Int)

testInputs = ["nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6"]

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
results insSet = swap . bimap (fst . head) (fst . head) . mapAccumL f [] . map (stepsFrom (length insSet - 1)) $ allInsSetList
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

findDupIndex :: [(Int, Int)] -> Maybe Int
findDupIndex [] = Nothing
findDupIndex (x:[]) = Nothing
findDupIndex es = search [] es
  where 
    search _ [] = Nothing
    search xs (y:ys) = let index = findIndex (\e -> snd e == snd y) xs 
                        in if index == Nothing 
                              then search (y:xs) ys 
                              else Just $ (length xs)

results2 :: [Entry] -> Answer
results2 insSet = (fst . last . stepsFrom cyclicPos $ insSet, fst . last . stepsFrom (insSetLength - 1) . switchInsAt changablePos $ insSet)
  where
    cyclicPos = last startReachableList
    changablePos = fst . head $ findConnectNodes startReachableList endReachableList newOps
    insSetLength = length insSet
    insGraph = buildG (0, insSetLength). insEdgesSet  $ zip [0..] insSet
    startReachableList = reachable insGraph 0
    endReachableList = reachable (transposeG insGraph) (insSetLength - 1)
    newOps = insEdgesSet $ allChangableOps insSet

stepsFrom stop insSet = steps 0 (0, 0)
  where
    steps value p
      | stop == fst p = []
      | otherwise = let pos = snd p in case insSet !! pos of 
                      Jmp n -> (value, pos) : steps value (pos, pos + n)
                      Acc n -> (value + n, pos) : steps (value + n) (pos, pos + 1)
                      Nop _ -> (value, pos) : steps value (pos, pos + 1)


allChangableOps :: [Instruction] -> [(Int, Instruction)]
allChangableOps = foldr (\(index, is) b -> case is of 
                            Nop n -> if n > 0 then (index, Jmp n):b else b 
                            Jmp n -> (index, Nop n):b
                            _ -> b) [] . zip [0..]

findConnectNodes startSet endSet = filter (\(u, v) -> u `elem` startSet && v `elem` endSet)

switchInsAt index insSet = updateList index  (case (insSet !! index) of
                                            Jmp n -> Nop n
                                            Nop n -> Jmp n) insSet

updateList :: Int -> a -> [a] -> [a]
updateList index v = toList . S.update index v . S.fromList

insEdgesSet :: [(Int, Instruction)] -> [(Int, Int)]
insEdgesSet [] = []
insEdgesSet ((index, is):insSet) = (case is of 
                                     Jmp m -> (index, index + m )
                                     _ -> (index, index + 1)) : insEdgesSet insSet
