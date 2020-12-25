#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = ([Int], [Int])
type Answer = (Int, Int)

main = do
  content <- getContents
  let (cardDeck1:cardDeck2:[]) = map parseRaw $ split "\n\n" content
      (w, res) = playCards cardDeck1 cardDeck2
      score = computeScore res
  putStrLn $ "Player " ++ show w ++ " is wining. Deck: " ++ show res
  putStrLn $ "Score (Part I): " ++ show score
  let (w2, res2) = playCards2 cardDeck1 cardDeck2
      score2 = computeScore res2
  putStrLn $ "Player " ++ show w2 ++ " is wining. Deck: " ++ show res2
  putStrLn $ "Score (Part II): " ++ show score2

parseRaw :: String -> [Int]
parseRaw = map read.tail.lines

playCards2 :: [Int] -> [Int] -> (Int, [Int])
playCards2 cd1 cd2 = playCards2_ cd1 cd2 [] []
  where
    playCards2_ [] cd2 _ _ = (2, cd2)
    playCards2_ cd1 [] _ _ = (1, cd1)
    playCards2_ cdd1@(d1:cd1) cdd2@(d2:cd2) c1 c2 =
      if cdd1 `elem` c1 || cdd2 `elem` c2
        then (1, cdd1++cdd2)
        else
          let isPlayOneWin = if d1 <= length cd1 && d2 <= length cd2
                               then let (p, _) = playCards2_ (take d1 cd1) (take d2 cd2) [] []
                                    in p == 1
                               else  d1 > d2
          in if isPlayOneWin
               then playCards2_ (cd1++[d1,d2]) cd2 (cdd1:c1) (cdd2:c2)
               else playCards2_ cd1 (cd2++[d2,d1]) (cdd1:c1) (cdd2:c2)

playCards :: [Int] -> [Int] -> (Int, [Int])
playCards [] cd2 = (2, cd2)
playCards cd1 [] = (1, cd1)
playCards (d1:cd1) (d2:cd2) =
  if d1 > d2
    then playCards (cd1++[d1,d2]) cd2
    else playCards cd1 (cd2++[d2,d1])

computeScore :: [Int] -> Int
computeScore cards = sum.zipWith (*) [1..length cards].reverse $ cards

-- helps
split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
