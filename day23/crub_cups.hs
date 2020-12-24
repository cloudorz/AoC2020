#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import qualified Data.IntMap.Strict as M

type Entry = Int
type Answer = (Int, Int)

type LinkedN = M.IntMap Int

main = do
  let t100 = transform2 100 9 4 (buildLinkedN inputs)
      finishedList = take 9 $ iterate (t100 M.!) 1
      t1m = transform2 10000000 1000000 4 (buildLinkedN (inputs++[10..1000000]))
      finishedList2 = take 3 $ iterate (t1m M.!) 1
  putStrLn ("1M 3 Numbers: " ++ show finishedList2)
  putStrLn ("Part I: " ++ (concat.map show $ tail finishedList))
  putStrLn ("Part II: " ++ (show.product.tail $ finishedList2))

inputs :: [Int]
inputs = [4, 6, 9, 2, 1, 7, 5, 3, 8]

testInputs :: [Int]
testInputs = [3, 8, 9, 1, 2, 5, 4, 6, 7]

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

results :: [Entry] -> Answer
results  = undefined

buildLinkedN :: [Int] -> LinkedN
buildLinkedN ns = M.fromList $ zip ns (tail ns ++ [head ns])

transform :: (Int, [Int]) -> (Int, [Int])
transform (current, ns) =
  let lenNs = length ns
      tailHeadNs = swapAt (succ current) ns
      currentCup = last tailHeadNs
      (pickedCups, ts) = splitAt 3 tailHeadNs
      preDestCup = maximum ([0..pred currentCup] \\ pickedCups)
      destCup = if preDestCup == 0 then maximum ts else preDestCup
      destCupIndex = fromJust $ elemIndex destCup ts
      (pres, succs) = splitAt (succ destCupIndex) ts
      newTailHeadNs = pres ++ pickedCups ++ succs
      updatedNs = swapAt (lenNs - succ current) newTailHeadNs
  in  (succ current `mod` lenNs, updatedNs)
  where
    swapAt n = uncurry (++).swap.splitAt n

transform2 :: Int -> Int -> Int -> LinkedN -> LinkedN
transform2 maxStep maxN currentCup m = transform2_ 1 currentCup m
  where
    maxSet = [maxN-3..maxN]
    transform2_ step currentCup m
      | step == maxStep + 1 = m
      | otherwise =
        let pickedUpStart = m M.! currentCup
            pickedUpMid = m M.! pickedUpStart
            pickedUpEnd = m M.! pickedUpMid
            pickedUPs = [pickedUpStart, pickedUpMid, pickedUpEnd]
            destCups = [max 1 (currentCup - 4)..(currentCup-1)] \\ pickedUPs
            destCup = maximum $ if null destCups then maxSet \\ pickedUPs else destCups
            newM = M.insert destCup pickedUpStart
              . M.insert pickedUpEnd (m M.! destCup)
              . M.insert currentCup (m M.! pickedUpEnd) $ m
        in transform2_ (succ step) (newM M.! currentCup) newM
