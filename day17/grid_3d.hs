#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- file contents
--
--
import Data.List (map, iterate, delete, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Entry = String
type Answer = (Int, Int)

type Space = M.Map Point Char
type Point = (Int, Int, Int, Int)

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

main = putStrLn . showResult . results $ inputs

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: "
  ++ show p1
  ++ ".\nPart II answer: "
  ++ show p2
  ++ ".\n"

results :: [Entry] -> Answer
results es = (spaceActivePointCount $ cubes zFilter es !! 6
             , spaceActivePointCount $ cubes id es !! 6)

spaceFromList :: [String] -> Space
spaceFromList =
  M.fromList . concat . map (\(i, s)
    -> map (\(j, c)
      -> ((i, j, 0, 0), c)) $ zip [0..] s) . zip [0..]

getV :: Space -> Point -> Char
getV sp point = fromMaybe '.' $ M.lookup point sp

setV :: Space -> Point -> Char -> Space
setV sp point v = M.insert point v sp

spaceActivePointCount :: Space -> Int
spaceActivePointCount = length . filter (== '#') . snd . unzip . M.toList

cubes :: ([Point] -> [Point]) -> [String] -> [Space]
cubes f inputs = map fst $
  iterate (newCubeFrom f) (spaceFromList inputs, (0, length inputs))

newCubeFrom :: ([Point] -> [Point])
  -> (Space, (Int, Int))
  -> (Space, (Int, Int))
newCubeFrom f (oldCube, (origin, size)) =
  (foldl' checkPoint M.empty . f $ xyzw [(origin-1)..(size-origin+1)]
  , (origin-1, size+2))
  where
    checkPoint newCube point =
      let currentV = getV oldCube point
          activeCount = length . filter (== '#')
            . map ((getV oldCube) . (plusP point))
            $ f nhoods
      in if currentV == '#'
           then if activeCount `elem` [2,3]
                  then setV newCube point '#'
                  else setV newCube point '.'
           else if activeCount == 3
                  then setV newCube point '#'
                  else setV newCube point '.'

zFilter :: [Point] -> [Point]
zFilter = filter (\(_, _, _, w) -> w == 0)

plusP :: Point -> Point -> Point
plusP !p1 !p2 = let (x, y, z, w) = p1
                    (x', y', z', w') = p2
                in (x+x', y+y', z+z', w+w')

nhoods :: [Point]
nhoods = delete (0, 0, 0, 0) $ xyzw [1, -1, 0]

xyzw :: [Int] -> [Point]
xyzw range = [(x', y', z', w')
  | x' <- range,
    y' <- range,
    z' <- range,
    w' <- range]
