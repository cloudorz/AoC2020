#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe

type Entry = Int
type Answer = (Int, Int)

type Space = M.Map Point Char
type Point = (Int, Int, Int)

inputs = [
  "...#...#",
  "..##.#.#",
  "###..#..",
  "........",
  "...##.#.",
  ".#.####.",
  "...####.",
  "..##...#"]

testInputs = [".#.", "..#", "###"]

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = undefined

results :: [Entry] -> Answer
results  = undefined

initialSpaceFromList :: [String] -> Space
initialSpaceFromList = M.fromList . concat . map (\(i, s) -> map (\(j, c) -> ((i, j, 0), c)) $ zip [0..] s) . zip [0..]

isActive sp point = '#' == getV sp point

getV :: Space -> Point -> Char
getV sp point = fromMaybe '.' $ M.lookup point sp

setV :: Space -> Point -> Char -> Space
setV sp point v = M.insert point v sp

cubes inputs = iterate newCubeFrom (initialSpaceFromList inputs, (0, length inputs))

newCubeFrom :: (Space, (Int, Int)) -> (Space, (Int, Int))
newCubeFrom (oldCube, (origin, size)) =
  let range = [(origin-1)..(size-origin+1)]
  in (foldl' (checkPoint oldCube) M.empty [(x', y', z') | x' <- range, y' <- range, z' <- range], (origin-1, size+2))

checkPoint oldCube newCube point =
  let currentV = getV oldCube point
      activeCount = length . filter (== '#') . map ((getV oldCube) . (plusP point)) $ nhoods
  in if currentV == '#'
       then if activeCount `elem` [2,3]
              then setV newCube point '#'
              else setV newCube point '.'
       else if activeCount == 3
              then setV newCube point '#'
              else setV newCube point '.'

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack

plusP p1 p2 = let (x, y, z) = p1
                  (x', y', z') = p2
              in (x+x', y+y', z+z')
nhoods =
  let ns = [1, -1, 0]
  in [(x, y, z) | x <- ns, y <- ns, z <- ns, not (x == 0 && y == 0 && z == 0)]
