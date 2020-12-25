#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)

type Entry = [String]
type Point = (Int, Int)
type TileMap = M.Map Point Int

main = do
  content <- getContents
  let es = map parseRaw . lines $ content
      ds = (fmap.fmap) directionToCoordinate es
      finalPoints = map (foldl' plusP (0, 0)) $ ds
      maxRadius = maximum.map (hexDistance (0, 0)) $ finalPoints
      tileMap = buildTileMap $ axialHexGridPoints maxRadius
      flipPoints = foldl1' (.) $ map flipTileState finalPoints
      identifiedTileMap = flipPoints tileMap
  putStrLn $ "Part I: " ++ (show.sum.M.elems $ identifiedTileMap)
  let days = take 101 $ iterate (exhibit.adjustHexGridSize) identifiedTileMap
  putStrLn "Part II: "
  putStrLn $ unlines.map (\(d, n) -> "Day " ++ show d ++ ": " ++ show n).zip [0..] $ (map (sum.M.elems) days)

parseRaw :: String -> Entry
parseRaw [] = []
parseRaw [s] = [[s]]
parseRaw (s1:s2:ss) = if [s1,s2] `elem` ["sw", "se", "nw", "ne"]
                        then [s1,s2]:parseRaw ss
                        else [s1]:parseRaw (s2:ss)

buildTileMap :: [Point] -> TileMap
buildTileMap points = M.fromList $ zip points (repeat 0)

exhibit :: TileMap -> TileMap
exhibit m = M.mapWithKey transform m
  where
    transform p s =
      let blackN = sum $ adjacentStates p m
      in if (s == 1 && (blackN == 0 || blackN > 2)) || (s == 0 && blackN == 2) then
           abs.pred $ s
         else s

adjustHexGridSize :: TileMap -> TileMap
adjustHexGridSize m =
  let getMaxRadius = maximum.map (hexDistance (0, 0)).M.keys
      maxRadius = getMaxRadius m
      maxBlackRadius = getMaxRadius $ M.filter (== 1) m
      saveRing = foldl1' (.).map ($ 0).map M.insert $ ring (0, (maxBlackRadius+1)) (maxBlackRadius+1)
  in if maxRadius > maxBlackRadius then m
     else saveRing m

axialHexGridPoints :: Int -> [Point]
axialHexGridPoints radius =
  let east = directionToCoordinate "e"
      eastPoints = scanl1 plusP $ replicate radius east
  in (0, 0):(concat $ zipWith ring eastPoints [1..radius])

ring :: Point -> Int -> [Point]
ring origin radius =
    let k = snd $ unzip directions
    in scanl' plusP origin $ concatMap (replicate radius) k

flipTileState :: Point -> TileMap -> TileMap
flipTileState point m =
  let value = fromJust.fmap (abs.pred) $ M.lookup point m
  in M.insert point value m

adjacentStates :: Point -> TileMap -> [Int]
adjacentStates point m = map (fromMaybe 0.((flip M.lookup) m).plusP point).snd.unzip $ directions

plusP :: Point -> Point -> Point
plusP (x, y) (x', y') = (x+x', y+y')

directions = [
  ("nw", (1, -1)),
  ("w", (0, -1)),
  ("sw", (-1, 0)),
  ("se", (-1, 1)),
  ("e", (0, 1)),
  ("ne", (1, 0))]

directionToCoordinate :: String -> Point
directionToCoordinate ds = fromJust $ lookup ds directions

hexDistance :: Point -> Point -> Int
hexDistance (x, y) (x0, y0) =
  (abs (x - x0) + abs (x+y-x0-y0) + abs (y - y0)) `div` 2
